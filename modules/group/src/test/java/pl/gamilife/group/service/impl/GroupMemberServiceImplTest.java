package pl.gamilife.group.service.impl;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.group.exception.domain.GroupFullException;
import pl.gamilife.group.exception.domain.UserAlreadyMemberOfGroupException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.shared.kernel.event.JoinedGroupEvent;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;

import java.time.Instant;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class GroupMemberServiceImplTest {

    @Mock
    private GroupMemberJpaRepository groupMemberRepository;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private UserApi userApi;

    @InjectMocks
    private GroupMemberServiceImpl groupMemberService;

    @Test
    void shouldCreateNewGroupMember_whenGroupIsNotFullAndUserIsNotMember() {
        // given
        UUID userId = UUID.randomUUID();
        BasicUserInfoDto userInfo = Instancio.create(BasicUserInfoDto.class);

        Group group = Instancio.of(Group.class)
                .set(field(Group::getMembersLimit), 10)
                .set(field("activeMembers"), Collections.emptySet())
                .create();

        when(userApi.getUserById(userId)).thenReturn(Optional.of(userInfo));
        when(groupMemberRepository.findByUserIdAndGroup(userId, group)).thenReturn(Optional.empty());
        when(groupMemberRepository.save(any(GroupMember.class))).thenAnswer(invocation -> invocation.getArgument(0));

        // when
        GroupMember result = groupMemberService.createGroupMember(group, userId);

        // then
        assertThat(result).isNotNull();
        assertThat(result.getGroup()).isEqualTo(group);
        assertThat(result.getUserId()).isEqualTo(userId);

        verify(groupMemberRepository).save(any(GroupMember.class));

        ArgumentCaptor<JoinedGroupEvent> eventCaptor = ArgumentCaptor.forClass(JoinedGroupEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());

        JoinedGroupEvent event = eventCaptor.getValue();
        assertThat(event.userId()).isEqualTo(userInfo.userId());
        assertThat(event.isFirstTimeJoin()).isTrue();
        assertThat(event.groupId()).isEqualTo(group.getId());
    }

    @Test
    void shouldReactivateMember_whenUserIsInactiveMember() {
        // given
        UUID userId = UUID.randomUUID();
        BasicUserInfoDto userInfo = Instancio.create(BasicUserInfoDto.class);

        Group group = Instancio.of(Group.class)
                .set(field(Group::getMembersLimit), 10)
                .set(field("activeMembers"), Collections.emptySet())
                .create();

        GroupMember inactiveMember = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getGroup), group)
                .set(field(GroupMember::getUserId), userId)
                .set(field("leftAt"), Instant.now()) // Inactive
                .create();

        when(userApi.getUserById(userId)).thenReturn(Optional.of(userInfo));
        when(groupMemberRepository.findByUserIdAndGroup(userId, group)).thenReturn(Optional.of(inactiveMember));
        when(groupMemberRepository.save(any(GroupMember.class))).thenAnswer(invocation -> invocation.getArgument(0));

        // when
        GroupMember result = groupMemberService.createGroupMember(group, userId);

        // then
        assertThat(result.isActive()).isTrue();
        assertThat(result.getUserId()).isEqualTo(userId);

        verify(groupMemberRepository).save(inactiveMember);

        ArgumentCaptor<JoinedGroupEvent> eventCaptor = ArgumentCaptor.forClass(JoinedGroupEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());

        JoinedGroupEvent event = eventCaptor.getValue();
        assertThat(event.isFirstTimeJoin()).isFalse();
    }

    @Test
    void shouldThrowException_whenGroupIsFull() {
        // given
        UUID userId = UUID.randomUUID();
        int limit = 2;

        // Create existing members to fill the group
        Set<GroupMember> members = IntStream.range(0, limit)
                .mapToObj(i -> Instancio.create(GroupMember.class))
                .collect(Collectors.toSet());

        Group group = Instancio.of(Group.class)
                .set(field(Group::getMembersLimit), limit)
                .set(field("activeMembers"), members)
                .create();

        // when
        Throwable thrown = catchThrowable(() -> groupMemberService.createGroupMember(group, userId));

        // then
        assertThat(thrown)
                .isInstanceOf(GroupFullException.class)
                .hasMessageContaining("is full");

        verify(groupMemberRepository, never()).save(any());
        verify(eventPublisher, never()).publishEvent(any());
    }

    @Test
    void shouldThrowException_whenUserNotFound() {
        // given
        UUID userId = UUID.randomUUID();
        Group group = Instancio.of(Group.class)
                .set(field(Group::getMembersLimit), 10)
                .set(field("activeMembers"), Collections.emptySet())
                .create();

        when(userApi.getUserById(userId)).thenReturn(Optional.empty());

        // when
        Throwable thrown = catchThrowable(() -> groupMemberService.createGroupMember(group, userId));

        // then
        assertThat(thrown)
                .isInstanceOf(UserNotFoundException.class)
                .hasMessageContaining("not found");

        verify(groupMemberRepository, never()).save(any());
    }

    @Test
    void shouldThrowException_whenUserIsAlreadyActiveMember() {
        // given
        UUID userId = UUID.randomUUID();
        BasicUserInfoDto userInfo = Instancio.create(BasicUserInfoDto.class);

        Group group = Instancio.of(Group.class)
                .set(field(Group::getMembersLimit), 10)
                .set(field("activeMembers"), Collections.emptySet())
                .create();

        GroupMember activeMember = Instancio.of(GroupMember.class)
                .set(field(GroupMember::getGroup), group)
                .set(field(GroupMember::getUserId), userId)
                .set(field("leftAt"), null) // Active
                .create();

        when(userApi.getUserById(userId)).thenReturn(Optional.of(userInfo));
        when(groupMemberRepository.findByUserIdAndGroup(userId, group)).thenReturn(Optional.of(activeMember));

        // when
        Throwable thrown = catchThrowable(() -> groupMemberService.createGroupMember(group, userId));

        // then
        assertThat(thrown)
                .isInstanceOf(UserAlreadyMemberOfGroupException.class)
                .hasMessageContaining("is already member of group");

        verify(groupMemberRepository, never()).save(any());
    }
}
package edu.pjwstk.groups.groupmember.creategroupmember;

import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.groups.entity.*;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.usecase.creategroupmember.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CreateGroupMemberInOpenGroupTest {

    @Mock
    GroupMemberRepository groupMemberRepository;

    @Mock
    GroupRepository groupRepository;

    @Mock
    UserApi userApi;

    @Mock
    CreateGroupMemberMapper createGroupMemberMapper;

    @InjectMocks
    CreateGroupMemberInOpenGroupUseCaseImpl createGroupMemberInOpenGroupUseCase;

    @Test
    void shouldCreateGroupMemberInOpenGroupSuccessfully() {

        UUID groupId = UUID.randomUUID();
        UUID userId = UUID.randomUUID();
        UUID groupMemberId = UUID.randomUUID();

        CreateGroupMemberRequest request = new CreateGroupMemberRequest(userId);

        GroupType groupType = GroupType.builder()
                .groupTypeId(1)
                .title("Open")
                .build();

        Group group = Group.builder()
                .groupId(groupId)
                .groupType(groupType)
                .membersLimit(5)
                .groupMembers(new ArrayList<>())
                .adminId(UUID.randomUUID())
                .build();

        BasicUserInfoApiDto userDto = new BasicUserInfoApiDto(
                userId,
                "test@mail.com",
                "testUser"
        );

        GroupMember newMember = GroupMember.builder()
                .groupMemberId(groupMemberId)
                .userId(userId)
                .memberGroup(group)
                .joinedAt(Instant.now())
                .groupMoney(0)
                .totalEarnedMoney(0)
                .build();

        CreateGroupMemberResponse.GroupDto groupDto =
                CreateGroupMemberResponse.GroupDto.builder()
                        .groupId(groupId)
                        .adminId(group.getAdminId())
                        .build();

        CreateGroupMemberResponse expectedResponse =
                CreateGroupMemberResponse.builder()
                        .groupMemberId(groupMemberId)
                        .memberGroup(groupDto)
                        .userId(userId)
                        .joinedAt(newMember.getJoinedAt())
                        .leftAt(null)
                        .groupMoney(0)
                        .totalEarnedMoney(0)
                        .build();

        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));
        when(userApi.getUserById(userId)).thenReturn(Optional.of(userDto));
        when(groupMemberRepository.findByUserIdAndGroup(group, userId)).thenReturn(Optional.empty());
        when(createGroupMemberMapper.toEntity(any(), any(), any())).thenReturn(newMember);
        when(groupMemberRepository.save(newMember)).thenReturn(newMember);
        when(createGroupMemberMapper.toResponse(newMember)).thenReturn(expectedResponse);

        CreateGroupMemberResponse result = createGroupMemberInOpenGroupUseCase.execute(request, groupId);

        assertThat(result).isNotNull();
        assertThat(result.userId()).isEqualTo(userId);
        assertThat(result.groupMemberId()).isEqualTo(groupMemberId);
        assertThat(result.memberGroup().groupId()).isEqualTo(groupId);
        assertThat(result.leftAt()).isNull();
        assertThat(result.groupMoney()).isZero();
        assertThat(result.totalEarnedMoney()).isZero();

        verify(groupRepository).findById(groupId);
        verify(userApi).getUserById(userId);
        verify(groupMemberRepository).findByUserIdAndGroup(group, userId);
        verify(groupMemberRepository).save(newMember);
        verify(createGroupMemberMapper).toEntity(eq(userId), eq(group), any(UUID.class));
        verify(createGroupMemberMapper).toResponse(newMember);
    }
}

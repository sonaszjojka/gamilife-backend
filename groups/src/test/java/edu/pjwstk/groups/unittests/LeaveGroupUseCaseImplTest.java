package edu.pjwstk.groups.unittests;

import edu.pjwstk.common.groupsApi.exception.GroupMemberNotFoundException;
import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.exception.AdministratorCannotLeaveGroupException;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.usecase.leavegroup.LeaveGroupMapper;
import edu.pjwstk.groups.usecase.leavegroup.LeaveGroupResponse;
import edu.pjwstk.groups.usecase.leavegroup.LeaveGroupUseCaseImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class LeaveGroupUseCaseImplTest {

    @Mock
    private GroupMemberRepository groupMemberRepository;

    @Mock
    private GroupRepository groupRepository;

    @Mock
    private LeaveGroupMapper leaveGroupMapper;

    @InjectMocks
    private LeaveGroupUseCaseImpl leaveGroupUseCase;

    private UUID groupMemberId;
    private UUID groupId;
    private UUID adminId;
    private GroupMember groupMember;
    private Group group;
    private LeaveGroupResponse response;

    @BeforeEach
    void setUp() {
        groupMemberId = UUID.randomUUID();
        groupId = UUID.randomUUID();
        adminId = UUID.randomUUID();

        groupMember = new GroupMember();
        groupMember.setGroupMemberId(groupMemberId);

        group = new Group();
        group.setGroupId(groupId);

        LeaveGroupResponse.GroupDto groupDto = LeaveGroupResponse.GroupDto.builder()
                .groupId(groupId)
                .adminId(adminId)
                .build();

        response = LeaveGroupResponse.builder()
                .groupMemberId(groupMemberId)
                .memberGroup(groupDto)
                .userId(UUID.randomUUID())
                .joinedAt(Instant.now().minusSeconds(3600))
                .leftAt(Instant.now())
                .groupMoney(100)
                .totalEarnedMoney(500)
                .build();
    }

    @Test
    void shouldSuccessfullyLeaveGroup_whenRegularMemberLeavesGroup() {
        // Given
        group.setAdminId(adminId);
        List<GroupMember> members = new ArrayList<>();
        members.add(groupMember);
        members.add(new GroupMember());
        group.setGroupMembers(members);

        Instant beforeExecution = Instant.now();

        when(groupMemberRepository.findById(groupMemberId)).thenReturn(Optional.of(groupMember));
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));
        when(groupMemberRepository.save(groupMember)).thenReturn(groupMember);
        when(leaveGroupMapper.toResponse(groupMember)).thenReturn(response);

        // When
        LeaveGroupResponse result = leaveGroupUseCase.execute(groupMemberId, groupId);

        // Then
        assertNotNull(result);
        assertEquals(response, result);
        assertNotNull(groupMember.getLeftAt());
        assertFalse(groupMember.getLeftAt().isBefore(beforeExecution));

        verify(groupMemberRepository).findById(groupMemberId);
        verify(groupRepository).findById(groupId);
        verify(groupMemberRepository).save(groupMember);
        verify(leaveGroupMapper).toResponse(groupMember);
    }

    @Test
    void shouldThrowGroupMemberNotFoundException_whenGroupMemberDoesNotExist() {
        // Given
        when(groupMemberRepository.findById(groupMemberId)).thenReturn(Optional.empty());

        // When
        GroupMemberNotFoundException exception = assertThrows(
                GroupMemberNotFoundException.class,
                () -> leaveGroupUseCase.execute(groupMemberId, groupId)
        );

        //Then
        assertEquals("Group member with id: " + groupMemberId + " not found!", exception.getMessage());
        verify(groupMemberRepository).findById(groupMemberId);
        verify(groupRepository, never()).findById(any());
        verify(groupMemberRepository, never()).save(any());
    }

    @Test
    void shouldThrowGroupNotFoundException_whenGroupDoesNotExist() {
        // Given
        when(groupMemberRepository.findById(groupMemberId)).thenReturn(Optional.of(groupMember));
        when(groupRepository.findById(groupId)).thenReturn(Optional.empty());

        // When
        GroupNotFoundException exception = assertThrows(
                GroupNotFoundException.class,
                () -> leaveGroupUseCase.execute(groupMemberId, groupId)
        );

        //Then
        assertEquals("Group with id: " + groupId + " not found!", exception.getMessage());
        verify(groupMemberRepository).findById(groupMemberId);
        verify(groupRepository).findById(groupId);
        verify(groupMemberRepository, never()).save(any());
    }

    @Test
    void shouldThrowAdministratorCannotLeaveGroupException_whenAdminTriesToLeaveGroupWithMultipleMembers() {
        // Given
        group.setAdminId(groupMemberId);
        List<GroupMember> members = new ArrayList<>();
        members.add(groupMember);
        members.add(new GroupMember());
        group.setGroupMembers(members);

        when(groupMemberRepository.findById(groupMemberId)).thenReturn(Optional.of(groupMember));
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));

        // When
        AdministratorCannotLeaveGroupException exception = assertThrows(
                AdministratorCannotLeaveGroupException.class,
                () -> leaveGroupUseCase.execute(groupMemberId, groupId)
        );

        //Then
        assertEquals("Administrator cannot leave group! Change group administrator before leaving.",
                exception.getMessage());
        verify(groupMemberRepository).findById(groupMemberId);
        verify(groupRepository).findById(groupId);
        verify(groupMemberRepository, never()).save(any());
    }

    @Test
    void shouldThrowAdministratorCannotLeaveGroupException_whenAdminIsOnlyMemberInGroup() {
        // Given
        group.setAdminId(groupMemberId);
        List<GroupMember> members = new ArrayList<>();
        members.add(groupMember);
        group.setGroupMembers(members);

        when(groupMemberRepository.findById(groupMemberId)).thenReturn(Optional.of(groupMember));
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));

        // When
        AdministratorCannotLeaveGroupException exception = assertThrows(
                AdministratorCannotLeaveGroupException.class,
                () -> leaveGroupUseCase.execute(groupMemberId, groupId)
        );

        //Then
        assertEquals("Administrator cannot leave group! Delete group instead of leaving.",
                exception.getMessage());
        verify(groupMemberRepository).findById(groupMemberId);
        verify(groupRepository).findById(groupId);
        verify(groupMemberRepository, never()).save(any());
    }


}
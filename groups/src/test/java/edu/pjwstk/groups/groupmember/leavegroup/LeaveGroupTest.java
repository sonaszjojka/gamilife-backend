package edu.pjwstk.groups.groupmember.leavegroup;


import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.usecase.leavegroup.LeaveGroupMapper;
import edu.pjwstk.groups.usecase.leavegroup.LeaveGroupResponse;
import edu.pjwstk.groups.usecase.leavegroup.LeaveGroupUseCaseImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class LeaveGroupTest {

    @Mock
    private GroupMemberRepository groupMemberRepository;

    @Mock
    private GroupRepository groupRepository;

    @Mock
    private LeaveGroupMapper leaveGroupMapper;

    @InjectMocks
    private LeaveGroupUseCaseImpl leaveGroupUseCase;

    @Test
    void shouldLeaveGroupSuccessfully() {
        // given
        UUID groupMemberId = UUID.randomUUID();
        UUID groupId = UUID.randomUUID();
        UUID userId = UUID.randomUUID();
        UUID adminId = UUID.randomUUID();

        GroupType groupType = GroupType.builder()
                .groupTypeId(1)
                .title("Open")
                .build();

        Group group = Group.builder()
                .groupId(groupId)
                .joinCode("ABC123XYZ456")
                .adminId(adminId)
                .groupCurrencySymbol('$')
                .membersLimit(10)
                .groupType(groupType)
                .build();

        GroupMember adminMember = GroupMember.builder()
                .groupMemberId(adminId)
                .userId(adminId)
                .memberGroup(group)
                .joinedAt(Instant.now().minusSeconds(30 * 24 * 60 * 60))
                .groupMoney(1000)
                .totalEarnedMoney(5000)
                .leftAt(null)
                .build();

        GroupMember regularMember = GroupMember.builder()
                .groupMemberId(groupMemberId)
                .userId(userId)
                .memberGroup(group)
                .joinedAt(Instant.now().minusSeconds(10 * 24 * 60 * 60))
                .groupMoney(500)
                .totalEarnedMoney(2000)
                .leftAt(null)
                .build();

        List<GroupMember> groupMembers = new ArrayList<>();
        groupMembers.add(adminMember);
        groupMembers.add(regularMember);
        group.setGroupMembers(groupMembers);

        GroupMember updatedMember = GroupMember.builder()
                .groupMemberId(regularMember.getGroupMemberId())
                .userId(regularMember.getUserId())
                .memberGroup(regularMember.getMemberGroup())
                .joinedAt(Instant.now().minusSeconds(10 * 24 * 60 * 60))
                .groupMoney(500)
                .totalEarnedMoney(2000)
                .leftAt(Instant.now())
                .build();

        LeaveGroupResponse.GroupDto groupDto = LeaveGroupResponse.GroupDto.builder()
                .groupId(groupId)
                .adminId(adminId)
                .build();

        LeaveGroupResponse expectedResponse = LeaveGroupResponse.builder()
                .groupMemberId(groupMemberId)
                .memberGroup(groupDto)
                .userId(userId)
                .joinedAt(regularMember.getJoinedAt())
                .leftAt(Instant.now())
                .groupMoney(500)
                .totalEarnedMoney(2000)
                .build();

        when(groupMemberRepository.findById(groupMemberId)).thenReturn(Optional.of(regularMember));
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));
        when(groupMemberRepository.save(any(GroupMember.class))).thenReturn(updatedMember);
        when(leaveGroupMapper.toResponse(any(GroupMember.class))).thenReturn(expectedResponse);


        LeaveGroupResponse result = leaveGroupUseCase.execute(groupMemberId, groupId);

        assertThat(result).isNotNull();
        assertThat(result.groupMemberId()).isEqualTo(groupMemberId);
        assertThat(result.userId()).isEqualTo(userId);
        assertThat(result.memberGroup()).isNotNull();
        assertThat(result.memberGroup().groupId()).isEqualTo(groupId);
        assertThat(result.memberGroup().adminId()).isEqualTo(adminId);
        assertThat(result.leftAt()).isNotNull();
        assertThat(result.joinedAt()).isNotNull();
        assertThat(result.groupMoney()).isEqualTo(500);
        assertThat(result.totalEarnedMoney()).isEqualTo(2000);

        ArgumentCaptor<GroupMember> groupMemberCaptor = ArgumentCaptor.forClass(GroupMember.class);
        verify(groupMemberRepository).save(groupMemberCaptor.capture());

        GroupMember capturedMember = groupMemberCaptor.getValue();
        assertThat(capturedMember.getLeftAt()).isNotNull();
        assertThat(capturedMember.getLeftAt()).isBeforeOrEqualTo(Instant.now());
        assertThat(capturedMember.getGroupMemberId()).isEqualTo(groupMemberId);


        verify(groupMemberRepository).findById(groupMemberId);
        verify(groupRepository).findById(groupId);
        verify(groupMemberRepository).save(any(GroupMember.class));
        verify(leaveGroupMapper).toResponse(updatedMember);
    }
}
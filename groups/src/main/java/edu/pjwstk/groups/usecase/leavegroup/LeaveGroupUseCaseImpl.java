package edu.pjwstk.groups.usecase.leavegroup;

import edu.pjwstk.core.exception.common.domain.GroupMemberNotFoundException;
import edu.pjwstk.groups.exception.domain.AdminCannotLeaveGroupException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.UUID;

@Service
@AllArgsConstructor
public class LeaveGroupUseCaseImpl implements LeaveGroupUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupJpaRepository groupRepository;

    @Override
    public LeaveGroupResult executeInternal(LeaveGroupCommand cmd) {
        GroupMember groupMember = getGroupMemberWithGroup(cmd.groupId(), cmd.groupMemberId());
        Group group = groupMember.getMemberGroup();

        if (group.isUserAdmin(groupMember.getUserId())) {
            throw new AdminCannotLeaveGroupException("Administrator cannot leave group!");
        }

        groupMember.setLeftAt(Instant.now());
        GroupMember savedGroupMember = groupMemberRepository.save(groupMember);

        return buildLeaveGroupResult(savedGroupMember);
    }

    private GroupMember getGroupMemberWithGroup(UUID groupId, UUID groupMemberId) {
        return groupMemberRepository.findWithMemberGroupByGroupMemberIdAndMemberGroup_GroupId(groupMemberId, groupId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: "
                        + groupMemberId + " not found!"));
    }

    private LeaveGroupResult buildLeaveGroupResult(GroupMember groupMember) {
        return LeaveGroupResult.builder()
                .groupMemberId(groupMember.getGroupMemberId())
                .memberGroup(
                        groupMember.getMemberGroup() != null
                                ? LeaveGroupResult.GroupDto.builder()
                                .groupId(groupMember.getMemberGroup().getGroupId())
                                .adminId(groupMember.getMemberGroup().getAdminId())
                                .build()
                                : null
                )
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}

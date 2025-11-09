package edu.pjwstk.groups.usecase.leavegroup;

import edu.pjwstk.core.exception.common.domain.GroupMemberNotFoundException;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
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
    public LeaveGroupResponse execute(UUID groupMemberId, UUID groupId) {
        Group group = getGroup(groupId);
        GroupMember groupMember = getGroupMember(groupId, groupMemberId);

        if (group.isUserAdmin(groupMember.getUserId())) {
            if (group.getGroupMembers().size() > 1) {
                throw new AdminCannotLeaveGroupException("Administrator cannot leave group! " +
                        "Change group administrator before leaving.");
            }
            throw new AdminCannotLeaveGroupException("Administrator cannot leave group! " +
                    "Delete group instead of leaving.");
        }

        groupMember.setLeftAt(Instant.now());
        GroupMember savedGroupMember = groupMemberRepository.save(groupMember);

        return buildLeaveGroupResponse(savedGroupMember);
    }

    private Group getGroup(UUID groupId) {
        return groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
    }

    private GroupMember getGroupMember(UUID groupId, UUID groupMemberId) {
        return groupMemberRepository.findByGroupMemberIdAndMemberGroup_GroupId(groupMemberId, groupId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: "
                        + groupMemberId + " not found!"));
    }

    private LeaveGroupResponse buildLeaveGroupResponse(GroupMember groupMember) {
        return LeaveGroupResponse.builder()
                .groupMemberId(groupMember.getGroupMemberId())
                .memberGroup(
                        groupMember.getMemberGroup() != null
                                ? LeaveGroupResponse.GroupDto.builder()
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

package edu.pjwstk.groups.usecase.leavegroup;

import pl.gamilife.infrastructure.core.exception.common.domain.GroupMemberNotFoundException;
import edu.pjwstk.groups.exception.domain.AdminCannotLeaveGroupException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.UUID;

@Service
@AllArgsConstructor
public class LeaveGroupUseCaseImpl implements LeaveGroupUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;

    @Override
    public LeaveGroupResult execute(LeaveGroupCommand cmd) {
        GroupMember groupMember = getGroupMemberWithGroup(cmd.groupId(), cmd.groupMemberId());
        Group group = groupMember.getGroup();

        if (group.isUserAdmin(groupMember.getUserId())) {
            throw new AdminCannotLeaveGroupException("Administrator cannot leave group!");
        }

        groupMember.setLeftAt(Instant.now());
        GroupMember savedGroupMember = groupMemberRepository.save(groupMember);

        return buildLeaveGroupResult(savedGroupMember);
    }

    private GroupMember getGroupMemberWithGroup(UUID groupId, UUID groupMemberId) {
        return groupMemberRepository.findWithGroupByGroupMemberIdAndGroupId(groupMemberId, groupId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: "
                        + groupMemberId + " not found!"));
    }

    private LeaveGroupResult buildLeaveGroupResult(GroupMember groupMember) {
        return LeaveGroupResult.builder()
                .groupMemberId(groupMember.getGroupMemberId())
                .group(new LeaveGroupResult.GroupDto(groupMember.getGroupId()))
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}

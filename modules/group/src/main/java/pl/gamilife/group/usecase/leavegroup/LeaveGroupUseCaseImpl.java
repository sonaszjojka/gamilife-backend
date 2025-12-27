package pl.gamilife.group.usecase.leavegroup;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.group.exception.domain.AdminCannotLeaveGroupException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;

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

        groupMember.leave();

        return buildLeaveGroupResult(groupMember);
    }

    private GroupMember getGroupMemberWithGroup(UUID groupId, UUID groupMemberId) {
        return groupMemberRepository.findWithGroupByIdAndGroupId(groupMemberId, groupId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: "
                        + groupMemberId + " not found!"));
    }

    private LeaveGroupResult buildLeaveGroupResult(GroupMember groupMember) {
        return LeaveGroupResult.builder()
                .groupMemberId(groupMember.getId())
                .group(new LeaveGroupResult.GroupDto(groupMember.getGroupId()))
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}

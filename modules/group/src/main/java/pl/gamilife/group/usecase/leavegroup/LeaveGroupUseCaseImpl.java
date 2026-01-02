package pl.gamilife.group.usecase.leavegroup;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.group.exception.domain.AdminCannotLeaveGroupException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.shared.kernel.event.GroupMemberLeftEvent;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class LeaveGroupUseCaseImpl implements LeaveGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public LeaveGroupResult execute(LeaveGroupCommand cmd) {
        Group group = groupRepository.findWithActiveMembersById(cmd.groupId()).orElseThrow(
                () -> new GroupNotFoundException("Group with id: " + cmd.groupId() + " not found!")
        );
        GroupMember groupMember = getGroupMemberWithGroup(cmd.groupId(), cmd.groupMemberId());

        if (!groupMember.isUser(cmd.userId()) && !group.isUserAdmin(cmd.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException(String.format(
                    "User %s does not have privileges to make Member %s leave Group %s",
                    cmd.userId(),
                    groupMember.getId(),
                    group.getId()
            ));
        }

        if (group.isUserAdmin(groupMember.getUserId())) {
            throw new AdminCannotLeaveGroupException("Administrator cannot leave group!");
        }

        groupMember.leave();

        eventPublisher.publishEvent(new GroupMemberLeftEvent(
                groupMember.getUserId(),
                groupMember.getGroupId(),
                groupMember.getGroup().getName(),
                group.getActiveMembers()
                        .stream()
                        .map(GroupMember::getUserId)
                        .filter(userId -> !userId.equals(groupMember.getUserId()))
                        .toList()
        ));

        return buildLeaveGroupResult(groupMember);
    }

    private GroupMember getGroupMemberWithGroup(UUID groupId, UUID groupMemberId) {
        return groupMemberRepository.findWithGroupByIdAndGroupId(groupMemberId, groupId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: "
                        + groupMemberId + " not found for group " + groupId));
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

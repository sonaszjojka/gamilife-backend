package pl.gamilife.group.usecase.editgroupmember;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.group.exception.domain.UserLeftGroupException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Service
@AllArgsConstructor
public class EditGroupMemberUseCaseImpl implements EditGroupMemberUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupJpaRepository groupRepository;

    @Override
    public EditGroupMemberResult execute(EditGroupMemberCommand cmd) {
        Group group = getGroup(cmd.groupId());
        GroupMember groupMember = getGroupMember(cmd.groupId(), cmd.groupMemberId());

        if (!group.isUserAdmin(cmd.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("Only group admin can edit group members!");
        }

        if (!groupMember.isActive()) {
            throw new UserLeftGroupException("Group member with id: " + cmd.groupMemberId() + " left group with id: "
                    + groupMember.getGroup().getId() + " and is no longer member of it!");
        }

        groupMember.setGroupMoney(cmd.groupMoney());

        return buildEditGroupMemberResult(groupMemberRepository.save(groupMember));
    }

    private Group getGroup(UUID groupId) {
        return groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
    }

    private GroupMember getGroupMember(UUID groupId, UUID groupMemberId) {
        return groupMemberRepository.findByIdAndGroupId(groupMemberId, groupId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: "
                        + groupMemberId + " not found!"));
    }

    private EditGroupMemberResult buildEditGroupMemberResult(GroupMember groupMember) {
        return EditGroupMemberResult.builder()
                .groupMemberId(groupMember.getId())
                .memberGroup(new EditGroupMemberResult.GroupDto(groupMember.getGroupId()))
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}

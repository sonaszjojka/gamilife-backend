package edu.pjwstk.groups.usecase.editgroupmember;

import pl.gamilife.infrastructure.core.exception.common.domain.GroupMemberNotFoundException;
import edu.pjwstk.groups.exception.domain.UserLeftGroupException;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class EditGroupMemberUseCaseImpl implements EditGroupMemberUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;

    @Override
    public EditGroupMemberResult execute(EditGroupMemberCommand cmd) {
        GroupMember groupMember = getGroupMember(cmd.groupId(), cmd.groupMemberId());

        if (!groupMember.isActive()) {
            throw new UserLeftGroupException("Group member with id: " + cmd.groupMemberId() + " left group with id: "
                    + groupMember.getGroup().getGroupId() + " and is no longer member of it!");
        }

        groupMember.setGroupMoney(cmd.groupMoney());
        groupMember.setTotalEarnedMoney(cmd.totalEarnedMoney());

        return buildEditGroupMemberResult(groupMemberRepository.save(groupMember));
    }

    private GroupMember getGroupMember(UUID groupId, UUID groupMemberId) {
        return groupMemberRepository.findByGroupMemberIdAndGroupId(groupMemberId, groupId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: "
                        + groupMemberId + " not found!"));
    }

    private EditGroupMemberResult buildEditGroupMemberResult(GroupMember groupMember) {
        return EditGroupMemberResult.builder()
                .groupMemberId(groupMember.getGroupMemberId())
                .memberGroup(new EditGroupMemberResult.GroupDto(groupMember.getGroupId()))
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}

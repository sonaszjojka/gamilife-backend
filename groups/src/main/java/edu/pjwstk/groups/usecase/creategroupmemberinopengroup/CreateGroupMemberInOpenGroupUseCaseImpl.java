package edu.pjwstk.groups.usecase.creategroupmemberinopengroup;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.groups.exception.domain.UserJoinGroupAccessDeniedException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.service.GroupMemberService;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@AllArgsConstructor
public class CreateGroupMemberInOpenGroupUseCaseImpl implements CreateGroupMemberInOpenGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final UserApi userApi;
    private final GroupMemberService groupMemberService;

    @Override
    @Transactional
    public CreateGroupMemberInOpenGroupResult executeInternal(CreateGroupMemberInOpenGroupCommand cmd) {
        Group group = getGroup(cmd.groupId());

        if (group.isOfType(GroupTypeEnum.OPEN)) {
            throw new UserJoinGroupAccessDeniedException("To add user to group which type is: REQUEST_ONLY or CLOSED " +
                    " - invitation or request must be accepted.");
        }

        userApi.getUserById(cmd.userId())
                .orElseThrow(() -> new UserNotFoundException("User with id: " + cmd.userId() + " not found!"));

        GroupMember groupMember = groupMemberService.createGroupMember(group, cmd.userId());
        return buildCreateGroupMemberResponse(groupMember);
    }

    private Group getGroup(UUID groupId) {
        return groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
    }

    private CreateGroupMemberInOpenGroupResult buildCreateGroupMemberResponse(GroupMember groupMember) {
        return CreateGroupMemberInOpenGroupResult.builder()
                .groupMemberId(groupMember.getGroupMemberId())
                .memberGroup(
                        groupMember.getMemberGroup() != null
                                ? CreateGroupMemberInOpenGroupResult.GroupDto.builder()
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

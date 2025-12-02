package edu.pjwstk.groups.usecase.creategroupmemberinopengroup;

import edu.pjwstk.api.user.UserApi;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupNotFoundException;
import pl.gamilife.infrastructure.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.groups.enums.GroupTypeEnum;
import edu.pjwstk.groups.exception.domain.UserJoinGroupAccessDeniedException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.service.GroupMemberService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class CreateGroupMemberInOpenGroupUseCaseImpl implements CreateGroupMemberInOpenGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final UserApi userApi;
    private final GroupMemberService groupMemberService;

    @Override
    public CreateGroupMemberInOpenGroupResult execute(CreateGroupMemberInOpenGroupCommand cmd) {
        Group group = getGroupWithMembers(cmd.groupId());

        if (!group.isOfType(GroupTypeEnum.OPEN)) {
            throw new UserJoinGroupAccessDeniedException("To add user to group which type is: REQUEST_ONLY or CLOSED " +
                    " - invitation or request must be accepted.");
        }

        userApi.getUserById(cmd.userId())
                .orElseThrow(() -> new UserNotFoundException("User with id: " + cmd.userId() + " not found!"));

        GroupMember groupMember = groupMemberService.createGroupMember(group, cmd.userId());
        return buildCreateGroupMemberResponse(groupMember);
    }

    private Group getGroupWithMembers(UUID groupId) {
        return groupRepository.findWithGroupMembersByGroupId(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
    }

    private CreateGroupMemberInOpenGroupResult buildCreateGroupMemberResponse(GroupMember groupMember) {
        return CreateGroupMemberInOpenGroupResult.builder()
                .groupMemberId(groupMember.getGroupMemberId())
                .memberGroup(new CreateGroupMemberInOpenGroupResult.GroupDto(
                        groupMember.getGroup().getGroupId(),
                        groupMember.getGroup().getAdminId()
                ))
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}

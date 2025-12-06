package pl.gamilife.group.usecase.creategroupmemberinopengroup;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.group.exception.domain.UserJoinGroupAccessDeniedException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.service.GroupMemberService;
import pl.gamilife.infrastructure.core.exception.domain.GroupNotFoundException;
import pl.gamilife.infrastructure.core.exception.domain.UserNotFoundException;

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

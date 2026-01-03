package pl.gamilife.group.usecase.creategroupinvitation;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.group.exception.domain.GroupFullException;
import pl.gamilife.group.exception.domain.UserAlreadyMemberOfGroupException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupInvitation;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupInvitationJpaRepository;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.service.GroupInvitationService;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class CreateGroupInvitationUseCaseImpl implements CreateGroupInvitationUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final GroupJpaRepository groupRepository;
    private final GroupInvitationService groupInvitationService;
    private final UserApi userApi;

    @Override
    public CreateGroupInvitationResult execute(CreateGroupInvitationCommand cmd) {
        Group group = getGroupWithMembers(cmd.groupId());
        BasicUserInfoDto userToInvite = getUserToInvite(cmd.targetUserId());

        if (!group.isUserAdmin(cmd.userId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can create group invitations!");
        }

        if (group.isFull()) {
            throw new GroupFullException("Group with id: " + group.getId() + " is full!");
        }

        boolean isUserAlreadyMember = group.getActiveMembers()
                .stream()
                .map(GroupMember::getUserId)
                .anyMatch(userId -> userId.equals(cmd.targetUserId()));
        if (isUserAlreadyMember) {
            throw new UserAlreadyMemberOfGroupException(String.format(
                    "User with id: %s is already member of group with id: %s",
                    userToInvite.userId(),
                    group.getId()
            ));
        }

        GroupInvitation groupInvitation = groupInvitationService.createGroupInvitation(group, userToInvite.userId());
        groupInvitationRepository.save(groupInvitation);

        return createResponse(groupInvitation);
    }

    private BasicUserInfoDto getUserToInvite(UUID userId) {
        return userApi.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User with id: " + userId + " not found!"));
    }

    private Group getGroupWithMembers(UUID groupId) {
        return groupRepository.findWithActiveMembersById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id:" + groupId + " not found!"));
    }

    private CreateGroupInvitationResult createResponse(GroupInvitation groupInvitation) {
        return CreateGroupInvitationResult.builder()
                .groupInvitationId(groupInvitation.getId())
                .groupInvited(new CreateGroupInvitationResult.GroupDto(
                        groupInvitation.getGroupId()
                ))
                .userId(groupInvitation.getUserId())
                .expiresAt(groupInvitation.getExpiresAt())
                .mailSentAt(groupInvitation.getCreatedAt())
                .invitationStatus(new CreateGroupInvitationResult.InvitationStatusDto(
                        groupInvitation.getStatus().getId(),
                        groupInvitation.getStatus().getTitle()
                ))
                .build();
    }
}

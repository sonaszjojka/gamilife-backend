package pl.gamilife.group.usecase.creategroupinvitation;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.group.exception.domain.GroupFullException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupInvitation;
import pl.gamilife.group.repository.GroupInvitationJpaRepository;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.service.GroupInvitationService;
import pl.gamilife.shared.kernel.event.GroupInvitationCreatedEvent;
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
    private final ApplicationEventPublisher eventPublisher;

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

        GroupInvitation groupInvitation = createGroupInvitation(group, userToInvite.userId());

        eventPublisher.publishEvent(new GroupInvitationCreatedEvent(
                userToInvite.userId(),
                groupInvitation.getLink(),
                group.getName(),
                group.getJoinCode()
        ));

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

    private GroupInvitation createGroupInvitation(Group group, UUID userId) {
        GroupInvitation groupInvitation = groupInvitationService.createGroupInvitation(group, userId);

        return groupInvitationRepository.save(groupInvitation);
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
                .link(groupInvitation.getLink())
                .invitationStatus(new CreateGroupInvitationResult.InvitationStatusDto(
                        groupInvitation.getStatus().getId(),
                        groupInvitation.getStatus().getTitle()
                ))
                .build();
    }
}

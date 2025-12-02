package edu.pjwstk.groups.usecase.creategroupinvitation;

import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamification.api.user.UserApi;
import pl.gamification.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.infrastructure.core.event.GroupInvitationCreatedEvent;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupNotFoundException;
import pl.gamilife.infrastructure.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.groups.enums.InvitationStatusEnum;
import edu.pjwstk.groups.exception.domain.GroupFullException;
import edu.pjwstk.groups.exception.domain.InvitationStatusNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.model.InvitationStatus;
import edu.pjwstk.groups.repository.GroupInvitationJpaRepository;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.InvitationStatusJpaRepository;
import edu.pjwstk.groups.util.GroupInvitationUtil;
import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class CreateGroupInvitationUseCaseImpl implements CreateGroupInvitationUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final InvitationStatusJpaRepository invitationStatusRepository;
    private final GroupJpaRepository groupRepository;
    private final AuthApi authApi;
    private final GroupInvitationUtil groupInvitationUtil;
    private final UserApi userApi;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public CreateGroupInvitationResult execute(CreateGroupInvitationCommand cmd) {
        CurrentUserDto adminDto = authApi.getCurrentUser();
        Group group = getGroupWithMembers(cmd.groupId());
        BasicUserInfoApiDto userToInvite = getUserToInvite(cmd.userId());

        if (!group.isUserAdmin(adminDto.userId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can create group invitations!");
        }

        if (group.isFull()) {
            throw new GroupFullException("Group with id: " + group.getGroupId() + " is full!");
        }

        InvitationStatus invitationStatus = getSentInvitationStatus();

        GroupInvitation groupInvitation = createGroupInvitation(group, invitationStatus, userToInvite.userId());

        eventPublisher.publishEvent(new GroupInvitationCreatedEvent(
                userToInvite.userId(),
                group.getJoinCode(),
                groupInvitation.getLink()
        ));

        return createResponse(groupInvitation);
    }

    private BasicUserInfoApiDto getUserToInvite(UUID userId) {
        return userApi.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User with id: " + userId + " not found!"));
    }

    private InvitationStatus getSentInvitationStatus() {
        return invitationStatusRepository.findById(InvitationStatusEnum.SENT.getId())
                .orElseThrow(() -> new InvitationStatusNotFoundException("Invitation stauts with id: "
                        + InvitationStatusEnum.SENT.getId() + " not found!"));
    }

    private Group getGroupWithMembers(UUID groupId) {
        return groupRepository.findWithGroupMembersByGroupId(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id:" + groupId + " not found!"));
    }

    private GroupInvitation createGroupInvitation(Group group, InvitationStatus invitationStatus, UUID userId) {
        UUID groupInvitationId = UUID.randomUUID();
        String token = groupInvitationUtil.generateToken();
        String hashedToken = groupInvitationUtil.hashToken(token);
        String link = groupInvitationUtil.generateGroupInvitationLink(group.getGroupId(), groupInvitationId, token);

        GroupInvitation groupInvitation = GroupInvitation.builder()
                .groupInvitationId(groupInvitationId)
                .userId(userId)
                .group(group)
                .expiresAt(groupInvitationUtil.calculateExpirationDate())
                .mailSentAt(LocalDateTime.now())
                .link(link)
                .tokenHash(hashedToken)
                .invitationStatus(invitationStatus)
                .build();

        return groupInvitationRepository.save(groupInvitation);
    }

    private CreateGroupInvitationResult createResponse(GroupInvitation groupInvitation) {
        return CreateGroupInvitationResult.builder()
                .groupInvitationId(groupInvitation.getGroupInvitationId())
                .groupInvited(new CreateGroupInvitationResult.GroupDto(
                        groupInvitation.getGroupId()
                ))
                .userId(groupInvitation.getUserId())
                .expiresAt(groupInvitation.getExpiresAt())
                .mailSentAt(groupInvitation.getMailSentAt())
                .link(groupInvitation.getLink())
                .invitationStatus(new CreateGroupInvitationResult.InvitationStatusDto(
                        groupInvitation.getInvitationStatus().getInvitationStatusId(),
                        groupInvitation.getInvitationStatus().getTitle()
                ))
                .build();
    }
}

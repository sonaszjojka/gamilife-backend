package edu.pjwstk.groups.usecase.creategroupinvitation;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.emailSender.EmailSenderApi;
import edu.pjwstk.api.emailSender.MailContentType;
import edu.pjwstk.api.emailSender.MailDto;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.core.exception.common.application.EmailSendingException;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.groups.exception.domain.GroupFullException;
import edu.pjwstk.groups.exception.domain.InvitationStatusNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.model.InvitationStatus;
import edu.pjwstk.groups.repository.GroupInvitationJpaRepository;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.InvitationStatusJpaRepository;
import edu.pjwstk.groups.shared.InvitationStatusEnum;
import edu.pjwstk.groups.util.GroupInvitationUtil;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@AllArgsConstructor
public class CreateGroupInvitationUseCaseImpl implements CreateGroupInvitationUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final InvitationStatusJpaRepository invitationStatusRepository;
    private final GroupJpaRepository groupRepository;
    private final AuthApi authApi;
    private final GroupInvitationUtil groupInvitationUtil;
    private final EmailSenderApi emailSenderApi;
    private final UserApi userApi;

    @Override
    @Transactional
    public CreateGroupInvitationResult executeInternal(CreateGroupInvitationCommand cmd) {
        CurrentUserDto adminDto = authApi.getCurrentUser();
        Group group = getGroup(cmd.groupId());
        BasicUserInfoApiDto userToInvite = getUserToInvite(cmd.userId());

        if (!group.isUserAdmin(adminDto.userId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can create group invitations!");
        }

        if (group.isFull()) {
            throw new GroupFullException("Group with id: " + group.getGroupId() + " is full!");
        }

        InvitationStatus invitationStatus = getSentInvitationStatus();

        GroupInvitation groupInvitation = createGroupInvitation(group, invitationStatus, userToInvite.userId());

        try {
            emailSenderApi.sendEmail(MailDto.builder()
                    .toEmail(userToInvite.email())
                    .subject(groupInvitationUtil.generateInvitationMailSubjectMessage())
                    .content(groupInvitationUtil.generateInvitationMailContentMessage(groupInvitation.getLink(), group.getJoinCode()))
                    .mailContentType(MailContentType.HTML)
                    .build());
        } catch (EmailSendingException e) {
            // TODO resend email or mark for resend
        }

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

    private Group getGroup(UUID groupId) {
        return groupRepository.findById(groupId)
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
                .groupInvited(group)
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
                .groupInvited(
                        groupInvitation.getGroupInvited() != null
                                ? CreateGroupInvitationResult.GroupDto.builder()
                                .groupId(groupInvitation.getGroupInvited().getGroupId())
                                .build()
                                : null
                )
                .userId(groupInvitation.getUserId())
                .expiresAt(groupInvitation.getExpiresAt())
                .mailSentAt(groupInvitation.getMailSentAt())
                .link(groupInvitation.getLink())
                .invitationStatus(
                        groupInvitation.getInvitationStatus() != null
                                ? CreateGroupInvitationResult.InvitationStatusDto.builder()
                                .invitationStatusId(groupInvitation.getInvitationStatus().getInvitationStatusId())
                                .title(groupInvitation.getInvitationStatus().getTitle())
                                .build()
                                : null
                )
                .build();
    }
}

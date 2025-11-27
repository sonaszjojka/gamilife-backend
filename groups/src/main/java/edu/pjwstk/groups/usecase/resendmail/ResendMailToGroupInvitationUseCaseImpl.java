package edu.pjwstk.groups.usecase.resendmail;

import edu.pjwstk.api.emailSender.EmailSenderApi;
import edu.pjwstk.api.emailSender.MailContentType;
import edu.pjwstk.api.emailSender.MailDto;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.core.exception.common.application.EmailSendingException;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.groups.exception.domain.GroupInvitationNotFoundException;
import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.repository.GroupInvitationJpaRepository;
import edu.pjwstk.groups.util.GroupInvitationUtil;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@AllArgsConstructor
public class ResendMailToGroupInvitationUseCaseImpl implements ResendMailToGroupInvitationUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final EmailSenderApi emailSenderApi;
    private final UserApi userApi;
    private final GroupInvitationUtil groupInvitationUtil;

    @Override
    @Transactional
    public Void executeInternal(ResendMailToGroupInvitationCommand cmd) {
        GroupInvitation groupInvitation = getGroupInvitationWithGroup(cmd.groupId(), cmd.groupInvitationId());
        BasicUserInfoApiDto invitedUserDto = getInvitedUser(groupInvitation);

        try {
            emailSenderApi.sendEmail(MailDto.builder()
                    .toEmail(invitedUserDto.email())
                    .subject(groupInvitationUtil.generateInvitationMailSubjectMessage())
                    .content(groupInvitationUtil.generateInvitationMailContentMessage(
                            groupInvitation.getLink(),
                            groupInvitation.getGroup().getJoinCode()))
                    .mailContentType(MailContentType.HTML)
                    .build());
        } catch (EmailSendingException e) {
            //todo - Resend if failed
        }

        return null;
    }

    private BasicUserInfoApiDto getInvitedUser(GroupInvitation groupInvitation) {
        return userApi.getUserById(groupInvitation.getUserId())
                .orElseThrow(
                        () -> new UserNotFoundException("User with id: " + groupInvitation.getUserId() + " not found!")
                );
    }

    private GroupInvitation getGroupInvitationWithGroup(UUID groupId, UUID groupInvitationId) {
        return groupInvitationRepository.findWithGroupByGroupInvitationIdAndGroupId(groupInvitationId, groupId)
                .orElseThrow(
                        () -> new GroupInvitationNotFoundException("Group invitation with id: " + groupInvitationId
                                + " not found!")
                );
    }
}

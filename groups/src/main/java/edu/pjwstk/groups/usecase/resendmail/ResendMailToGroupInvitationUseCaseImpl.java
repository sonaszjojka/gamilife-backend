package edu.pjwstk.groups.usecase.resendmail;

import edu.pjwstk.api.emailSender.EmailSenderApi;
import edu.pjwstk.core.exception.common.application.EmailSendingException;
import edu.pjwstk.api.emailSender.MailContentType;
import edu.pjwstk.api.emailSender.MailDto;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.groups.entity.GroupInvitation;
import edu.pjwstk.groups.exception.GroupInvitationNotFoundException;
import edu.pjwstk.groups.repository.GroupInvitationRepository;
import edu.pjwstk.groups.util.GroupInvitationUtil;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
public class ResendMailToGroupInvitationUseCaseImpl implements ResendMailToGroupInvitationUseCase {

    private final GroupInvitationRepository groupInvitationRepository;
    private final EmailSenderApi emailSenderApi;
    private final UserApi userApi;
    private final GroupInvitationUtil groupInvitationUtil;

    public ResendMailToGroupInvitationUseCaseImpl(GroupInvitationRepository groupInvitationRepository, EmailSenderApi emailSenderApi, UserApi userApi, GroupInvitationUtil groupInvitationUtil) {
        this.groupInvitationRepository = groupInvitationRepository;
        this.emailSenderApi = emailSenderApi;
        this.userApi = userApi;
        this.groupInvitationUtil = groupInvitationUtil;
    }

    @Override
    @Transactional
    public void execute(UUID groupId, UUID groupInvitationId) {
        GroupInvitation groupInvitation = groupInvitationRepository.findById(groupInvitationId)
                .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id: " + groupInvitationId
                        + " not found!"));

        BasicUserInfoApiDto basicUserInfoApiDto = userApi.getUserById(groupInvitation.getUserId())
                .orElseThrow();

        try {
            emailSenderApi.sendEmail(MailDto.builder()
                    .toEmail(basicUserInfoApiDto.email())
                    .subject(groupInvitationUtil.generateInvitationMailSubjectMessage())
                    .content(groupInvitationUtil.generateInvitationMailContentMessage(
                            groupInvitation.getLink(),
                            groupInvitation.getGroupInvited().getJoinCode()))
                    .mailContentType(MailContentType.HTML)
                    .build());
        } catch (EmailSendingException e) {
            //todo - ?
            throw new RuntimeException(e);
        }
    }
}

package pl.gamilife.communication.usecase.senduseremail;

import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.communication.dto.EmailParameters;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import sendinblue.ApiClient;
import sendinblue.Configuration;
import sendinblue.auth.ApiKeyAuth;
import sibApi.TransactionalEmailsApi;
import sibModel.CreateSmtpEmail;
import sibModel.SendSmtpEmail;
import sibModel.SendSmtpEmailSender;
import sibModel.SendSmtpEmailTo;

import java.util.List;
import java.util.UUID;

@Service
@Slf4j
@RequiredArgsConstructor
public class SendUserEmailUseCaseImpl implements SendUserEmailUseCase {

    private final UserApi userApi;

    @Value("${brevo.api-key}")
    private String brevoApiKey;

    @Value("${brevo.from-email}")
    private String fromEmail;

    @Value("${brevo.from-name}")
    private String fromName;

    @Value("${brevo.templates.verification-email}")
    private long verificationEmailId;

    @Value("${brevo.templates.reset-password-email}")
    private long resetPasswordEmailId;

    @Value("${brevo.templates.group-invitation-email}")
    private long groupInvitationEmailId;

    @Override
    public Void execute(SendUserEmailCommand cmd) {
        BasicUserInfoDto user = getUser(cmd.userId());
        sendEmail(cmd.emailParameters(), user);

        return null;
    }

    private void sendEmail(@NotNull EmailParameters emailParameters, BasicUserInfoDto user) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();

        ApiKeyAuth apiKey = (ApiKeyAuth) defaultClient.getAuthentication("api-key");
        apiKey.setApiKey(brevoApiKey);

        SendSmtpEmailTo to = new SendSmtpEmailTo();
        to.setEmail(user.email());
        to.setName(user.username());

        SendSmtpEmailSender sender = new SendSmtpEmailSender();
        sender.setEmail(fromEmail);
        sender.setName(fromName);

        TransactionalEmailsApi emailsApi = new TransactionalEmailsApi();
        SendSmtpEmail sendSmtpEmail = new SendSmtpEmail();

        sendSmtpEmail.setTo(List.of(to));
        sendSmtpEmail.setTemplateId(switch (emailParameters.getEmailType()) {
            case EMAIL_VERIFICATION -> verificationEmailId;
            case FORGOT_PASSWORD -> resetPasswordEmailId;
            case GROUP_INVITATION -> groupInvitationEmailId;
        });

        sendSmtpEmail.setParams(emailParameters.getParametersMap());
        sendSmtpEmail.setSender(sender);

        try {
            CreateSmtpEmail createSmtpEmail = emailsApi.sendTransacEmail(sendSmtpEmail);
            log.debug("Email sent to user {}. Message ID: {}", user.username(), createSmtpEmail.getMessageId());
        } catch (Exception e) {
            log.error("Error sending {} email to user", emailParameters.getEmailType(), e);
        }
    }

    private BasicUserInfoDto getUser(UUID userId) {
        return userApi.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }
}

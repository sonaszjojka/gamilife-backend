package pl.gamilife.communication.util;

import pl.gamilife.communication.dto.*;
import org.springframework.stereotype.Service;
import pl.gamilife.communication.dto.*;

@Service
public class EmailContentCreatorImpl implements EmailContentCreator {

    @Override
    public EmailContent createContentForParameters(EmailParameters parameters) {
        return switch (parameters.getEmailType()) {
            case EMAIL_VERIFICATION -> getContentForEmailVerification((EmailVerificationEmailParameters) parameters);
            case FORGOT_PASSWORD -> getContentForForgotPassword((ForgotPasswordEmailParameters) parameters);
            case GROUP_INVITATION -> getContentForGroupInvitation((GroupInvitationEmailParameters) parameters);
        };
    }

    private EmailContent getContentForEmailVerification(EmailVerificationEmailParameters parameters) {
        String content = """
                <html>
                  <body>
                    <p>Hello!</p>
                    <p>Please verify your email by clicking the link below:</p>
                    <p><a href="%s">Verify Email</a></p>
                    <p>If you did not request this, you can ignore this email.</p>
                    <br />
                    <p>Best regards,<br />GamiLife Team</p>
                  </body>
                </html>
                """.formatted(parameters.verificationLink());

        return EmailContent.builder()
                .contentType("text/html")
                .subject("Verify your email address")
                .content(content)
                .build();
    }

    private EmailContent getContentForForgotPassword(ForgotPasswordEmailParameters parameters) {
        String content = """
                <html>
                  <body>
                    <p>Hi,</p>
                    <p>In order to reset your password, please click the button below:</p>
                    <p>
                      <a
                        href="%s"
                        style="
                          display: inline-block;
                          padding: 10px 20px;
                          background-color: #1890ff;
                          color: white;
                          text-decoration: none;
                          border-radius: 4px;
                        "
                        >Reset Password</a
                      >
                    </p>
                    <p>If you did not request this, you can safely ignore this email.</p>
                    <br />
                    <p>Best Regards,<br />GamiLife Team</p>
                  </body>
                </html>
                
                """.formatted(parameters.resetLink());

        return EmailContent.builder()
                .contentType("text/html")
                .subject("Reset your password")
                .content(content)
                .build();
    }

    private EmailContent getContentForGroupInvitation(GroupInvitationEmailParameters parameters) {
        String content = """
                <html>
                  <body>
                    <p>You have been invited to join the group with code: <b>%s</b>.</p>
                    <p>
                      Click the link below to open the invitation page and decide whether to
                      join:
                    </p>
                    <p><a href="%s">Join the group</a></p>
                  </body>
                </html>
                """.formatted(parameters.joinCode(), parameters.invitationLink());

        return EmailContent.builder()
                .contentType("text/html")
                .subject("You've been invited to a new group")
                .content(content)
                .build();
    }
}

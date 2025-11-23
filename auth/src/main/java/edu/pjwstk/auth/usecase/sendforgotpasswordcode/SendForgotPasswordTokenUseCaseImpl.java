package edu.pjwstk.auth.usecase.sendforgotpasswordcode;

import edu.pjwstk.api.emailSender.EmailSenderApi;
import edu.pjwstk.core.exception.common.application.EmailSendingException;
import edu.pjwstk.api.emailSender.MailContentType;
import edu.pjwstk.api.emailSender.MailDto;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.auth.exception.domain.CannotCurrentlyCreateNewForgotPasswordCodeException;
import edu.pjwstk.auth.models.ForgotPasswordCode;
import edu.pjwstk.auth.repository.JpaForgotPasswordCodeRepository;
import edu.pjwstk.auth.service.ForgotPasswordCodeService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class SendForgotPasswordTokenUseCaseImpl implements SendForgotPasswordTokenUseCase {

    private final ForgotPasswordCodeService forgotPasswordCodeService;
    private final UserApi userApi;
    private final EmailSenderApi emailSenderApi;
    private final JpaForgotPasswordCodeRepository forgotPasswordCodeRepository;

    @Value("${app.frontend-urls.reset-password-url}")
    private String resetPasswordUrl;

    @Value("${app.frontend-urls.main-url}")
    private String appUrl;


    @Override
    @Transactional
    public Boolean executeInternal(SendForgotPasswordCodeCommand cmd) {
        Optional<BasicUserInfoApiDto> foundUser = userApi.getUserByEmail(cmd.email());
        if (foundUser.isEmpty()) {
            // No exception for security reasons
            return false;
        }
        BasicUserInfoApiDto user = foundUser.get();

        List<ForgotPasswordCode> codes = forgotPasswordCodeRepository
                .findByUserIdAndRevoked(
                        user.userId(),
                        false,
                        Sort.by(Sort.Direction.DESC, "issuedAt")
                );

        if (!forgotPasswordCodeService.checkIfCanResendForgotPasswordCode(codes)) {
            throw new CannotCurrentlyCreateNewForgotPasswordCodeException(
                    "Cannot currently create a new forgot password code. Please try again later."
            );
        }

        if (!codes.isEmpty()) {
            forgotPasswordCodeRepository.revokeAllActiveForgotPasswordCodesByUserId(user.userId());
        }

        String code = forgotPasswordCodeService.generateAndSaveForgotPasswordCode(user.userId());

        String resetLink = String.format("%s%s?code=%s", appUrl, resetPasswordUrl, code);

        String htmlContent = """
            <html>
              <body>
                <p>Hi,</p>
                <p>In order to reset your password, please click the button below:</p>
                <p><a href="%s" style="display:inline-block;padding:10px 20px;background-color:#1890ff;color:white;text-decoration:none;border-radius:4px;">Reset Password</a></p>
                <p>If you did not request this, you can safely ignore this email.</p>
                <br/>
                <p>Best Regards,<br/>GamiLife Team</p>
              </body>
            </html>
            """.formatted(resetLink);

        try {
            emailSenderApi.sendEmail(new MailDto(
                    cmd.email(),
                    "Reset your password",
                    htmlContent,
                    MailContentType.HTML
            ));
            return true;
        } catch (EmailSendingException ignored) {
            // TODO: resend logic
            return false;
        }
    }
}

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
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@AllArgsConstructor
public class SendForgotPasswordTokenUseCaseImpl implements SendForgotPasswordTokenUseCase {

    private final ForgotPasswordCodeService forgotPasswordCodeService;
    private final UserApi userApi;
    private final EmailSenderApi emailSenderApi;
    private final JpaForgotPasswordCodeRepository forgotPasswordCodeRepository;

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
            throw new CannotCurrentlyCreateNewForgotPasswordCodeException("Cannot currently create a new forgot password code. Please try again later.");
        }

        if (!codes.isEmpty()) {
            forgotPasswordCodeRepository.revokeAllActiveForgotPasswordCodesByUserId(user.userId());
        }

        String code = forgotPasswordCodeService.generateAndSaveForgotPasswordCode(user.userId());
        try {
            emailSenderApi.sendEmail(new MailDto(
                            cmd.email(),
                            "Reset your password",
                            """
                                    Hi,
                                    
                                    In order to reset your password use this link:
                                    """ + code +
                                    """
                                            \n
                                            Best Regards,
                                            GamiLife Team
                                            """,
                            MailContentType.TEXT
                    )
            );
            return true;
        } catch (EmailSendingException ignored) {
            // TODO: resend logic
            return false;
        }
    }
}

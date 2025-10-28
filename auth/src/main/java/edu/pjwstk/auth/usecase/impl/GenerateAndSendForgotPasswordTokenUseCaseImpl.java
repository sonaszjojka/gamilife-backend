package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.domain.ForgotPasswordCode;
import edu.pjwstk.auth.exceptions.CannotCurrentlyCreateNewForgotPasswordCodeException;
import edu.pjwstk.auth.persistence.repository.ForgotPasswordCodeRepository;
import edu.pjwstk.auth.usecase.GenerateAndSendForgotPasswordTokenUseCase;
import edu.pjwstk.auth.util.ForgotPasswordCodeUtil;
import edu.pjwstk.common.emailSenderApi.EmailSenderApi;
import edu.pjwstk.common.emailSenderApi.EmailSendingException;
import edu.pjwstk.common.emailSenderApi.MailContentType;
import edu.pjwstk.common.emailSenderApi.MailDto;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@AllArgsConstructor
public class GenerateAndSendForgotPasswordTokenUseCaseImpl implements GenerateAndSendForgotPasswordTokenUseCase {

    private final ForgotPasswordCodeUtil forgotPasswordCodeUtil;
    private final UserApi userApi;
    private final EmailSenderApi emailSenderApi;
    private final ForgotPasswordCodeRepository forgotPasswordCodeRepository;
    private final long forgotPasswordCodeTimeout;
    private final long forgotPasswordCodeResendInterval;


    @Override
    @Transactional
    public void execute(String email) {
        Optional<BasicUserInfoApiDto> foundUser = userApi.getUserByEmail(email);
        if (foundUser.isEmpty()) {
            // No exception for security reasons
            return;
        }
        BasicUserInfoApiDto user = foundUser.get();

        List<ForgotPasswordCode> codes = forgotPasswordCodeRepository
                .findByUserIdAndNotRevokedOrderByIssuedAt(user.userId());

        // Do not resend a code if user has a non revoked code that will not expire
        // in the duration of the resend interval period
        if (!codes.isEmpty() &&
                codes.getFirst()
                        .expiresAt()
                        .minusSeconds(forgotPasswordCodeResendInterval)
                        .isAfter(LocalDateTime.now())
        ) {
            throw new CannotCurrentlyCreateNewForgotPasswordCodeException("You have to wait a before you can get a new code");
        }

        String code = forgotPasswordCodeUtil.generateCode();
        ForgotPasswordCode forgotPasswordCode = new ForgotPasswordCode(
                UUID.randomUUID(),
                user.userId(),
                forgotPasswordCodeUtil.hashCode(code),
                LocalDateTime.now(),
                LocalDateTime.now().plusSeconds(forgotPasswordCodeTimeout),
                false
        );

        forgotPasswordCodeRepository.create(forgotPasswordCode);

        try {
            emailSenderApi.sendEmail(new MailDto(
                            email,
                            "Reset your password",
                            """
                                    Hi,
                                    
                                    In order to reset your password use this link:\n
                                    """ + code +
                                    """
                                            \nBest Regards,
                                            GamiLife Team
                                            """,
                            MailContentType.TEXT
                    )
            );
        } catch (EmailSendingException e) {
            // TODO: maybe resend logic
            return;
        }

    }
}

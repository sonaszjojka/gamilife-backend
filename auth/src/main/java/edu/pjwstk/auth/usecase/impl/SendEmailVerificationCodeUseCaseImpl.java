package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.exceptions.CannotCurrentlyCreateNewEmailVerificationCodeException;
import edu.pjwstk.auth.exceptions.EmailAlreadyVerifiedException;
import edu.pjwstk.auth.models.EmailVerificationCode;
import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.usecase.SendEmailVerificationCodeUseCase;
import edu.pjwstk.auth.service.EmailVerificationCodeService;
import edu.pjwstk.api.emailSender.EmailSenderApi;
import edu.pjwstk.api.emailSender.EmailSendingException;
import edu.pjwstk.api.emailSender.MailContentType;
import edu.pjwstk.api.emailSender.MailDto;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.CheckIfUsersEmailIsVerifiedApiDto;
import lombok.AllArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@AllArgsConstructor
public class SendEmailVerificationCodeUseCaseImpl implements SendEmailVerificationCodeUseCase {

    private final UserApi userApi;
    private final EmailSenderApi emailSenderApi;
    private final JpaEmailVerificationRepository emailVerificationRepository;
    private final EmailVerificationCodeService emailVerificationCodeService;
    private final long emailVerificationTimeout;
    private final long emailVerificationResendInterval;

    @Override
    public void execute(UUID userId) {
        CheckIfUsersEmailIsVerifiedApiDto dto = userApi.checkIfUsersEmailIsVerified(userId);
        if (dto.isVerified()) {
            throw new EmailAlreadyVerifiedException("Email already verified");
        }

        List<EmailVerificationCode> codes = emailVerificationRepository
                .findByUserIdAndRevokedOrderByIssuedAtDesc(userId, false);

        // Do not resend a code if user has a non revoked code that will not expire
        // in the duration of the resend interval period
        if (!codes.isEmpty() &&
                codes.getFirst()
                        .getExpiresAt()
                        .minusSeconds(emailVerificationResendInterval)
                        .isAfter(LocalDateTime.now())
        ) {
            throw new CannotCurrentlyCreateNewEmailVerificationCodeException("You have to wait a before you can get a new code");
        }

        if (!codes.isEmpty()) {
            emailVerificationRepository.revokeAllActiveEmailVerificationCodesByUserId(userId);
        }

        String code = UUID.randomUUID().toString();

        EmailVerificationCode emailVerification = new EmailVerificationCode(
                UUID.randomUUID(),
                userId,
                emailVerificationCodeService.hashCode(code),
                LocalDateTime.now(),
                LocalDateTime.now().plusSeconds(emailVerificationTimeout),
                false
        );

        emailVerificationRepository.save(emailVerification);

        try {
            emailSenderApi.sendEmail(new MailDto(
                    dto.email(),
                    "Email verification code",
                    """
                            Hello!
                            
                            Here is your verification code:
                            """ +
                            code +
                            """
                                    \n
                                    Best regards,
                                    GamiLife Team
                                    """,
                    MailContentType.TEXT
            ));
        } catch (EmailSendingException e) {
            throw new RuntimeException(e);
        }
    }
}

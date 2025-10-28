package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.domain.EmailVerification;
import edu.pjwstk.auth.exceptions.CannotCurrentlyCreateNewEmailVerificationCodeException;
import edu.pjwstk.auth.exceptions.EmailAlreadyVerifiedException;
import edu.pjwstk.auth.persistence.repository.EmailVerificationRepository;
import edu.pjwstk.auth.usecase.SendEmailVerificationCodeUseCase;
import edu.pjwstk.auth.util.VerificationCodeUtil;
import edu.pjwstk.common.emailSenderApi.EmailSenderApi;
import edu.pjwstk.common.emailSenderApi.EmailSendingException;
import edu.pjwstk.common.emailSenderApi.MailContentType;
import edu.pjwstk.common.emailSenderApi.MailDto;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.CheckIfUsersEmailIsVerifiedApiDto;
import lombok.AllArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@AllArgsConstructor
public class SendEmailVerificationCodeUseCaseImpl implements SendEmailVerificationCodeUseCase {

    private final UserApi userApi;
    private final EmailSenderApi emailSenderApi;
    private final EmailVerificationRepository emailVerificationRepository;
    private final VerificationCodeUtil verificationCodeUtil;
    private final long emailVerificationTimeout;
    private final long emailVerificationResendInterval;

    @Override
    public void execute(UUID userId) {
        CheckIfUsersEmailIsVerifiedApiDto dto = userApi.checkIfUsersEmailIsVerified(userId);
        if (dto.isVerified()) {
            throw new EmailAlreadyVerifiedException("Email already verified");
        }

        List<EmailVerification> codes = emailVerificationRepository
                .findByUserIdAndNotRevokedOrderByIssuedAt(userId);

        // Do not resend a code if user has a non revoked code that will not expire
        // in the duration of the resend interval period
        if (!codes.isEmpty() &&
                codes.getFirst()
                        .expiresAt()
                        .minusSeconds(emailVerificationResendInterval)
                        .isAfter(LocalDateTime.now())
        ) {
            throw new CannotCurrentlyCreateNewEmailVerificationCodeException("You have to wait a before you can get a new code");
        }

        if (!codes.isEmpty()) {
            emailVerificationRepository.revokeAllActiveEmailVerificationCodesByUserId(userId);
        }

        String code = UUID.randomUUID().toString();

        EmailVerification emailVerification = new EmailVerification(
                UUID.randomUUID(),
                userId,
                verificationCodeUtil.hashCode(code),
                LocalDateTime.now(),
                LocalDateTime.now().plusSeconds(emailVerificationTimeout),
                false
        );

        emailVerificationRepository.create(emailVerification);

        try {
            emailSenderApi.sendEmail(new MailDto(
                    dto.email(),
                    "Email verification code",
                    """
                            Hello!
                            
                            Here is your verification code:\n
                            """ +
                            code +
                            """
                                    \nBest regards,
                                    GamiLife Team
                                    """,
                    MailContentType.TEXT
            ));
        } catch (EmailSendingException e) {
            throw new RuntimeException(e);
        }
    }
}

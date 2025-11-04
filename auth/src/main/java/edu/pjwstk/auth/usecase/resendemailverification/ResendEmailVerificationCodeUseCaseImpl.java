package edu.pjwstk.auth.usecase.resendemailverification;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.CheckIfUsersEmailIsVerifiedApiDto;
import edu.pjwstk.auth.exceptions.CannotCurrentlyCreateNewEmailVerificationCodeException;
import edu.pjwstk.auth.exceptions.EmailAlreadyVerifiedException;
import edu.pjwstk.auth.models.EmailVerificationCode;
import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.service.EmailVerificationService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@AllArgsConstructor
public class ResendEmailVerificationCodeUseCaseImpl implements ResendEmailVerificationCodeUseCase {

    private final UserApi userApi;
    private final JpaEmailVerificationRepository emailVerificationRepository;
    private final EmailVerificationService emailVerificationService;


    @Override
    public void execute(UUID userId) {
        CheckIfUsersEmailIsVerifiedApiDto dto = userApi.checkIfUsersEmailIsVerified(userId);
        if (dto.isVerified()) {
            throw new EmailAlreadyVerifiedException("Email already verified");
        }

        List<EmailVerificationCode> codes = emailVerificationRepository
                .findByUserIdAndRevokedOrderByIssuedAtDesc(userId, false);

        if (emailVerificationService.checkIfCanResendEmailVerificationCode(codes)) {
            throw new CannotCurrentlyCreateNewEmailVerificationCodeException("You have to wait a before you can get a new code");
        }

        if (!codes.isEmpty()) {
            emailVerificationRepository.revokeAllActiveEmailVerificationCodesByUserId(userId);
        }

        String code = emailVerificationService.generateAndSaveEmailVerificationCode(userId);
        emailVerificationService.sendEmailVerificationCode(userId, dto.email(), code);
    }
}

package edu.pjwstk.auth.usecase.resendemailverification;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.CheckIfUsersEmailIsVerifiedApiDto;
import edu.pjwstk.auth.exception.domain.CannotCurrentlyCreateNewEmailVerificationCodeException;
import edu.pjwstk.auth.exception.domain.EmailAlreadyVerifiedException;
import edu.pjwstk.auth.models.EmailVerificationCode;
import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.service.EmailVerificationService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@AllArgsConstructor
public class ResendEmailVerificationCodeUseCaseImpl implements ResendEmailVerificationCodeUseCase {

    private final UserApi userApi;
    private final JpaEmailVerificationRepository emailVerificationRepository;
    private final EmailVerificationService emailVerificationService;


    @Override
    public Void executeInternal(ResendEmailVerificationCodeCommand cmd) {
        CheckIfUsersEmailIsVerifiedApiDto dto = userApi.checkIfUsersEmailIsVerified(cmd.userId());
        if (dto.isVerified()) {
            throw new EmailAlreadyVerifiedException("Email already verified");
        }

        List<EmailVerificationCode> codes = emailVerificationRepository
                .findByUserIdAndRevokedOrderByIssuedAtDesc(cmd.userId(), false);

        if (!emailVerificationService.checkIfCanResendEmailVerificationCode(codes)) {
            throw new CannotCurrentlyCreateNewEmailVerificationCodeException("You have to wait a before you can get a new code");
        }

        if (!codes.isEmpty()) {
            emailVerificationRepository.revokeAllActiveEmailVerificationCodesByUserId(cmd.userId());
        }

        String code = emailVerificationService.generateAndSaveEmailVerificationCode(cmd.userId());
        emailVerificationService.sendEmailVerificationCode(cmd.userId(), dto.email(), code);

        return null;
    }
}

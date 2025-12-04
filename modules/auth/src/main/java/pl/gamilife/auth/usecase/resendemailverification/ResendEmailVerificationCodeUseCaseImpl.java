package pl.gamilife.auth.usecase.resendemailverification;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.CheckIfUsersEmailIsVerifiedApiDto;
import pl.gamilife.auth.exception.domain.CannotCurrentlyCreateNewEmailVerificationCodeException;
import pl.gamilife.auth.exception.domain.EmailAlreadyVerifiedException;
import pl.gamilife.auth.models.EmailVerificationCode;
import pl.gamilife.auth.repository.JpaEmailVerificationRepository;
import pl.gamilife.auth.service.EmailVerificationService;

import java.util.List;

@Service
@Transactional
@AllArgsConstructor
public class ResendEmailVerificationCodeUseCaseImpl implements ResendEmailVerificationCodeUseCase {

    private final UserApi userApi;
    private final JpaEmailVerificationRepository emailVerificationRepository;
    private final EmailVerificationService emailVerificationService;


    @Override
    public Void execute(ResendEmailVerificationCodeCommand cmd) {
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

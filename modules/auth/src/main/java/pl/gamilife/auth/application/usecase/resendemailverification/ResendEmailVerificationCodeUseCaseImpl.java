package pl.gamilife.auth.application.usecase.resendemailverification;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.domain.exception.domain.CannotCurrentlyCreateNewEmailVerificationCodeException;
import pl.gamilife.auth.domain.exception.domain.EmailAlreadyVerifiedException;
import pl.gamilife.auth.domain.model.EmailVerificationCode;
import pl.gamilife.auth.domain.model.projection.EmailVerificationUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.port.repository.EmailVerificationRepository;
import pl.gamilife.auth.domain.service.EmailVerificationService;

import java.util.List;

@Service
@Transactional
@AllArgsConstructor
public class ResendEmailVerificationCodeUseCaseImpl implements ResendEmailVerificationCodeUseCase {

    private final UserContext userContext;
    private final EmailVerificationRepository emailVerificationRepository;
    private final EmailVerificationService emailVerificationService;


    @Override
    public Void execute(ResendEmailVerificationCodeCommand cmd) {
        EmailVerificationUserDetails dto = userContext.checkIfUsersEmailIsVerified(cmd.userId());
        if (dto.verified()) {
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

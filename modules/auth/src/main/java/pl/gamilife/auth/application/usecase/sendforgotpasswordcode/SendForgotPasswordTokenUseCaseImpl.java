package pl.gamilife.auth.application.usecase.sendforgotpasswordcode;

import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.domain.exception.domain.CannotCurrentlyCreateNewForgotPasswordCodeException;
import pl.gamilife.auth.domain.model.ForgotPasswordCode;
import pl.gamilife.auth.domain.model.projection.BasicUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.port.repository.ForgotPasswordCodeRepository;
import pl.gamilife.auth.domain.service.ForgotPasswordCodeService;
import pl.gamilife.shared.kernel.event.ForgotPasswordCodeRequestedEvent;

import java.util.List;
import java.util.Optional;

@Service
@Transactional
@RequiredArgsConstructor
public class SendForgotPasswordTokenUseCaseImpl implements SendForgotPasswordTokenUseCase {

    private final ForgotPasswordCodeService forgotPasswordCodeService;
    private final UserContext userContext;
    private final ForgotPasswordCodeRepository forgotPasswordCodeRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public Void execute(SendForgotPasswordCodeCommand cmd) {
        Optional<BasicUserDetails> foundUser = userContext.getUserByEmail(cmd.email());
        if (foundUser.isEmpty()) {
            // No exception for security reasons
            return null;
        }
        BasicUserDetails user = foundUser.get();

        List<ForgotPasswordCode> codes = forgotPasswordCodeRepository
                .findNewestByUserIdAndRevoked(
                        user.userId(),
                        false
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

        eventPublisher.publishEvent(new ForgotPasswordCodeRequestedEvent(user.userId(), code));

        return null;
    }
}

package pl.gamilife.auth.usecase.sendforgotpasswordcode;

import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.auth.exception.domain.CannotCurrentlyCreateNewForgotPasswordCodeException;
import pl.gamilife.auth.models.ForgotPasswordCode;
import pl.gamilife.auth.repository.JpaForgotPasswordCodeRepository;
import pl.gamilife.auth.service.ForgotPasswordCodeService;
import pl.gamilife.infrastructure.core.event.PasswordResetRequestedEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
@Transactional
@RequiredArgsConstructor
public class SendForgotPasswordTokenUseCaseImpl implements SendForgotPasswordTokenUseCase {

    private final ForgotPasswordCodeService forgotPasswordCodeService;
    private final UserApi userApi;
    private final JpaForgotPasswordCodeRepository forgotPasswordCodeRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Value("${app.frontend-urls.reset-password-url}")
    private String resetPasswordUrl;

    @Value("${app.frontend-urls.main-url}")
    private String appUrl;


    @Override
    public Boolean execute(SendForgotPasswordCodeCommand cmd) {
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
        eventPublisher.publishEvent(new PasswordResetRequestedEvent(user.userId(), resetLink));

        return null;
    }
}

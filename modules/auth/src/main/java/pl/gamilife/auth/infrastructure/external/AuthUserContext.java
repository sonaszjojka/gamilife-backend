package pl.gamilife.auth.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.api.user.dto.CheckIfUsersEmailIsVerifiedDto;
import pl.gamilife.api.user.dto.RegisterUserDto;
import pl.gamilife.auth.domain.model.projection.BasicUserDetails;
import pl.gamilife.auth.domain.model.projection.EmailVerificationUserDetails;
import pl.gamilife.auth.domain.model.projection.RegisterUserDetails;
import pl.gamilife.auth.domain.model.projection.SecureUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;

import java.util.Optional;
import java.util.UUID;

@Component
@AllArgsConstructor
public class AuthUserContext implements UserContext {

    private final UserApi userApi;

    @Override
    public Optional<SecureUserDetails> getSecureUserDataById(UUID userId) {
        return userApi.getSecureUserDataById(userId).flatMap(u -> Optional.of(new SecureUserDetails(
                u.userId(),
                u.email(),
                u.username(),
                u.password(),
                u.isEmailVerified(),
                u.isTutorialCompleted(),
                u.money()
        )));
    }

    @Override
    public Optional<SecureUserDetails> getSecureUserDataByEmail(String email) {
        return userApi.getSecureUserDataByEmail(email).flatMap(u -> Optional.of(new SecureUserDetails(
                u.userId(),
                u.email(),
                u.username(),
                u.password(),
                u.isEmailVerified(),
                u.isTutorialCompleted(),
                u.money()
        )));
    }

    @Override
    public BasicUserDetails confirmUserEmailVerification(UUID userId) {
        BasicUserInfoDto dto = userApi.confirmUserEmailVerification(userId);

        return new BasicUserDetails(
                dto.userId(),
                dto.email(),
                dto.username(),
                dto.level(),
                dto.experience(),
                dto.money()
        );
    }

    @Override
    public Optional<BasicUserDetails> getUserByEmail(String email) {
        return userApi.getUserByEmail(email).flatMap(u -> Optional.of(new BasicUserDetails(
                u.userId(),
                u.email(),
                u.username(),
                u.level(),
                u.experience(),
                u.money()
        )));
    }

    @Override
    public BasicUserDetails registerNewUser(RegisterUserDetails user) {
        BasicUserInfoDto dto = userApi.registerNewUser(new RegisterUserDto(
                user.firstName(),
                user.lastName(),
                user.email(),
                user.password(),
                user.username(),
                user.dateOfBirth(),
                user.sendBudgetReports(),
                user.isProfilePublic(),
                user.isEmailVerified(),
                user.isTutorialCompleted(),
                user.zoneId()
        ));

        return new BasicUserDetails(
                dto.userId(),
                dto.email(),
                dto.username(),
                dto.level(),
                dto.experience(),
                dto.money()
        );
    }

    @Override
    public EmailVerificationUserDetails checkIfUsersEmailIsVerified(UUID userId) {
        CheckIfUsersEmailIsVerifiedDto dto = userApi.checkIfUsersEmailIsVerified(userId);
        return new EmailVerificationUserDetails(dto.email(), dto.isVerified());
    }

    @Override
    public void updateUserEmail(UUID userId, String newEmail) {
        userApi.updateUserEmail(userId, newEmail);
    }

    @Override
    public void updateUserPassword(UUID userId, String encodedPassword) {
        userApi.updateUserPassword(userId, encodedPassword);
    }
}

package pl.gamilife.user.api;

import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.*;
import pl.gamilife.user.dto.service.UserDetails;
import pl.gamilife.user.usecase.*;
import pl.gamilife.user.usecase.editusermoney.EditUserMoneyCommand;
import pl.gamilife.user.usecase.editusermoney.EditUserMoneyUseCase;
import pl.gamilife.user.usecase.getcurrentuserdatetime.GetCurrentUserZoneIdCommand;
import pl.gamilife.user.usecase.getcurrentuserdatetime.GetCurrentUserZoneIdUseCase;
import pl.gamilife.user.usecase.grantrewardstouser.GrantRewardsToUserCommand;
import pl.gamilife.user.usecase.grantrewardstouser.GrantRewardsToUserUseCase;
import pl.gamilife.user.usecase.levelupuser.LevelUpUserCommand;
import pl.gamilife.user.usecase.levelupuser.LevelUpUserUseCase;

import java.time.ZoneId;
import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class UserApiImpl implements UserApi {

    private final CheckIfUsersEmailIsVerifiedUseCase checkIfUsersEmailIsVerifiedUseCase;
    private final ConfirmUserEmailVerificationUseCase confirmUserEmailVerificationUseCase;
    private final GetSecureUserDataByEmailUseCase getSecureUserDataByEmailUseCase;
    private final GetSecureUserDataByIdUseCase getSecureUserDataByIdUseCase;
    private final GetUserByEmailUseCase getUserByEmailUseCase;
    private final GetUserByIdUseCase getUserByIdUseCase;
    private final RegisterNewUserUseCase registerNewUserUseCase;
    private final UpdateUserEmailUseCase updateUserEmailUseCase;
    private final UpdateUserPasswordUseCase updateUserPasswordUseCase;
    private final EditUserMoneyUseCase editUserMoneyUseCase;
    private final GrantRewardsToUserUseCase grantRewardsToUserUseCase;
    private final LevelUpUserUseCase levelUpUserUseCase;
    private final GetCurrentUserZoneIdUseCase getCurrentUserZoneIdUseCase;
    private final GetUserDetailsUseCase getUserDetailsUseCase;

    @Override
    @Transactional
    public BasicUserInfoDto registerNewUser(RegisterUserDto user) {
        return registerNewUserUseCase.execute(user);
    }

    @Override
    public Optional<BasicUserInfoDto> getUserById(UUID userId) {
        return getUserByIdUseCase.execute(userId);
    }

    @Override
    public Optional<BasicUserInfoDto> getUserByEmail(String email) {
        return getUserByEmailUseCase.execute(email);
    }

    @Override
    public Optional<SecureUserInfoDto> getSecureUserDataById(UUID userId) {
        return getSecureUserDataByIdUseCase.execute(userId);
    }

    @Override
    public Optional<SecureUserInfoDto> getSecureUserDataByEmail(String email) {
        return getSecureUserDataByEmailUseCase.execute(email);
    }

    @Override
    public void updateUserEmail(UUID userId, String newEmail) {
        updateUserEmailUseCase.execute(userId, newEmail);
    }

    @Override
    public CheckIfUsersEmailIsVerifiedDto checkIfUsersEmailIsVerified(UUID userId) {
        return checkIfUsersEmailIsVerifiedUseCase.execute(userId);
    }

    public BasicUserInfoDto confirmUserEmailVerification(UUID userId) {
        return confirmUserEmailVerificationUseCase.execute(userId);
    }

    @Override
    public void updateUserPassword(UUID userId, String hashedNewPassword) {
        updateUserPasswordUseCase.execute(userId, hashedNewPassword);
    }

    @Override
    public int editUserMoneyBy(UUID userId, Integer money) {
        return editUserMoneyUseCase.execute(new EditUserMoneyCommand(userId, money));
    }

    @Override
    public RewardedUserApiDto grantRewardsToUser(UUID userId, int experience, int money) {
        return grantRewardsToUserUseCase.execute(new GrantRewardsToUserCommand(
                userId,
                experience,
                money
        ));
    }

    @Override
    public void levelUpUser(UUID userId, int level) {
        levelUpUserUseCase.execute(new LevelUpUserCommand(userId, level));
    }

    @Override
    public ZoneId getUserZoneId(UUID userId) {
        return getCurrentUserZoneIdUseCase.execute(new GetCurrentUserZoneIdCommand(userId));
    }

    @Override
    public UserDetailsDto getUserDetails(UUID userId) {
        UserDetails user = getUserDetailsUseCase.execute(userId);
        return new UserDetailsDto(
                user.id(),
                user.firstName(),
                user.lastName(),
                user.email(),
                user.username(),
                user.dateOfBirth(),
                user.experience(),
                user.level(),
                user.money(),
                user.sendBudgetReports(),
                user.isProfilePublic(),
                user.isEmailVerified(),
                user.isTutorialCompleted()
        );
    }
}

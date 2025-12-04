package pl.gamilife.api.user;

import pl.gamilife.api.user.dto.*;

import java.util.Optional;
import java.util.UUID;

public interface UserApi {
    BasicUserInfoApiDto registerNewUser(RegisterUserApiDto user);

    Optional<BasicUserInfoApiDto> getUserById(UUID userId);

    Optional<BasicUserInfoApiDto> getUserByEmail(String email);

    Optional<SecureUserInfoApiDto> getSecureUserDataById(UUID userId);

    Optional<SecureUserInfoApiDto> getSecureUserDataByEmail(String email);

    void updateUserEmail(UUID userId, String newEmail);

    CheckIfUsersEmailIsVerifiedApiDto checkIfUsersEmailIsVerified(UUID userId);

    BasicUserInfoApiDto confirmUserEmailVerification(UUID userId);

    void resetUserPassword(UUID userId, String hashedNewPassword);

    int editUserMoneyBy(UUID userId, Integer money);

    RewardedUserApiDto grantRewardsToUser(UUID userId, int experience, int money);

    void levelUpUser(UUID userId, int level);
}

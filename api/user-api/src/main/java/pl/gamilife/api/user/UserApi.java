package pl.gamilife.api.user;

import pl.gamilife.api.user.dto.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

public interface UserApi {
    BasicUserInfoDto registerNewUser(RegisterUserDto user);

    Optional<BasicUserInfoDto> getUserById(UUID userId);

    Optional<BasicUserInfoDto> getUserByEmail(String email);

    Optional<SecureUserInfoDto> getSecureUserDataById(UUID userId);

    Optional<SecureUserInfoDto> getSecureUserDataByEmail(String email);

    void updateUserEmail(UUID userId, String newEmail);

    CheckIfUsersEmailIsVerifiedDto checkIfUsersEmailIsVerified(UUID userId);

    BasicUserInfoDto confirmUserEmailVerification(UUID userId);

    void updateUserPassword(UUID userId, String hashedNewPassword);

    int editUserMoneyBy(UUID userId, Integer money);

    RewardedUserApiDto grantRewardsToUser(UUID userId, int experience, int money);

    void levelUpUser(UUID userId, int level);

    LocalDate getCurrentUserDate(UUID userId);

    LocalDateTime getCurrentUserDateTime(UUID userId);
}

package edu.pjwstk.api.user;

import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.api.user.dto.CheckIfUsersEmailIsVerifiedApiDto;
import edu.pjwstk.api.user.dto.RegisterUserApiDto;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;

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
}

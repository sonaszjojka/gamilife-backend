package edu.pjwstk.auth.services;

import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.LoginUserDto;
import edu.pjwstk.auth.dto.service.RegisterUserDto;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;

public interface AuthService {
    BasicUserInfoApiDto registerUser(RegisterUserDto registerUserDto);

    AuthTokens loginUser(LoginUserDto loginUserDto);

    void logoutUser(String token);

    AuthTokens refreshAccessToken(String refreshToken);

    AuthTokens issueJwtTokens(BasicUserInfoApiDto user);

    boolean checkPassword(String providedPassword, String hashedPassword);
}

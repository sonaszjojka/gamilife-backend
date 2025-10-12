package edu.pjwstk.auth.services.impl;

import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.LoginUserDto;
import edu.pjwstk.auth.dto.service.RefreshToken;
import edu.pjwstk.auth.dto.service.RegisterUserDto;
import edu.pjwstk.auth.exceptions.*;
import edu.pjwstk.auth.persistence.repository.RefreshTokenRepository;
import edu.pjwstk.auth.services.AuthService;
import edu.pjwstk.auth.util.TokenProvider;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.dto.RegisterUserApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.security.InvalidParameterException;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
public class AuthServiceImpl implements AuthService {

    private final UserApi userApi;
    private final RefreshTokenRepository refreshTokenRepository;
    private final BCryptPasswordEncoder passwordEncoder;
    private final TokenProvider tokenProvider;

    public AuthServiceImpl(
            UserApi userApi,
            RefreshTokenRepository refreshTokenRepository,
            BCryptPasswordEncoder passwordEncoder,
            TokenProvider tokenProvider
    ) {
        this.userApi = userApi;
        this.refreshTokenRepository = refreshTokenRepository;
        this.passwordEncoder = passwordEncoder;
        this.tokenProvider = tokenProvider;
    }

    @Override
    @Transactional
    public BasicUserInfoApiDto registerUser(RegisterUserDto registerUserDto) {
        if (!validatePassword(registerUserDto.password())) {
            throw new InvalidParameterException("Password must be at least 8 characters long, contain at least one letter, one digit, and one special character.");
        }

        RegisterUserApiDto user = new RegisterUserApiDto(
                registerUserDto.firstName(),
                registerUserDto.lastName(),
                registerUserDto.email(),
                passwordEncoder.encode(registerUserDto.password()),
                registerUserDto.username(),
                registerUserDto.dateOfBirth(),
                registerUserDto.sendBudgetReports(),
                registerUserDto.isProfilePublic()
        );

        return userApi.registerNewUser(user);
    }

    @Override
    @Transactional
    public AuthTokens loginUser(LoginUserDto loginUserDto) {
        BasicUserInfoApiDto user = userApi
                .getUserByEmail(loginUserDto.email())
                .orElseThrow(() -> new InvalidCredentialsException("Login credentials are invalid"));

        if (!checkPassword(loginUserDto.password(), user.password())) {
            throw new InvalidCredentialsException("Login credentials are invalid");
        }

        return generateNewAuthTokens(user);
    }

    @Override
    public void logoutUser(String token) {
        if (token.isBlank()) {
            throw new RefreshTokenNotProvidedException("Refresh token is blank value");
        }
        String hashedToken = tokenProvider.hashToken(token);

        RefreshToken refreshToken = refreshTokenRepository
                .getRefreshTokenByHashedToken(hashedToken)
                .orElseThrow(() -> new RefreshTokenUnknownException("Refresh token not found"));

        if (!refreshToken.expiresAt().isBefore(LocalDateTime.now()) && !refreshToken.revoked()) {
            refreshTokenRepository.updateRevokedStatus(refreshToken.id(), true);
        }
    }

    @Override
    @Transactional
    public AuthTokens refreshAccessToken(String refreshToken) {
        String hashedRefreshToken = tokenProvider.hashToken(refreshToken);
        RefreshToken existingRefreshToken = refreshTokenRepository
                .getRefreshTokenByHashedToken(hashedRefreshToken)
                .orElseThrow(() -> new RefreshTokenUnknownException("Refresh token not found"));

        if (existingRefreshToken.revoked()) {
            throw new RefreshTokenRevokedException("Refresh token has been revoked");
        }

        if (existingRefreshToken.expiresAt().isBefore(LocalDateTime.now())) {
            throw new RefreshTokenExpiredException("Refresh token has expired");
        }

        BasicUserInfoApiDto user = userApi.getUserById(existingRefreshToken.userId())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return new AuthTokens(
                generateAccessToken(user),
                refreshToken
        );
    }

    @Override
    @Transactional
    public AuthTokens issueJwtTokens(BasicUserInfoApiDto user) {
        if (user == null) {
            throw new UserNotFoundException("User not found");
        }

        return generateNewAuthTokens(user);
    }

    @Override
    public boolean checkPassword(String providedPassword, String hashedPassword) {
        return passwordEncoder.matches(providedPassword, hashedPassword);
    }

    @Transactional
    protected AuthTokens generateNewAuthTokens(BasicUserInfoApiDto user) {
        String accessToken = generateAccessToken(user);
        String rawRefreshToken = tokenProvider.generateRefreshToken();
        String hashedRefreshToken = tokenProvider.hashToken(rawRefreshToken);
        RefreshToken refreshToken = new RefreshToken(
                UUID.randomUUID(),
                user.userId(),
                hashedRefreshToken,
                LocalDateTime.now(),
                LocalDateTime.now().plusSeconds(tokenProvider.getRefreshTokenExpirationTime()),
                false
        );

        refreshTokenRepository.save(refreshToken);

        return new AuthTokens(accessToken, rawRefreshToken);
    }

    private boolean validatePassword(String password) {
        if (password == null || password.length() < 8) {
            return false;
        }

        boolean hasSpecial = password.matches(".*[!@#$%^&*()_+\\-={}:;\"'\\[\\]|<>,.?/~`].*");
        boolean hasLetter = password.matches(".*[A-Za-z].*");
        boolean hasDigit = password.matches(".*\\d.*");

        return hasSpecial && hasLetter && hasDigit;
    }

    private String generateAccessToken(BasicUserInfoApiDto user) {
        return tokenProvider.generateToken(user.userId(), user.email());
    }
}


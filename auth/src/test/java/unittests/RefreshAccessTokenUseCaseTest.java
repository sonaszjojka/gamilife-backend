package unittests;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.auth.exception.domain.InvalidRefreshTokenException;
import edu.pjwstk.auth.exception.domain.RefreshTokenExpiredException;
import edu.pjwstk.auth.models.RefreshToken;
import edu.pjwstk.auth.repository.JpaRefreshTokenRepository;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.refreshtoken.RefreshAccessTokenCommand;
import edu.pjwstk.auth.usecase.refreshtoken.RefreshAccessTokenUseCaseImpl;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Instant;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class RefreshAccessTokenUseCaseTest {

    @Mock
    private TokenService tokenService;

    @Mock
    private JpaRefreshTokenRepository refreshTokenRepository;

    @Mock
    private UserApi userApi;

    @InjectMocks
    private RefreshAccessTokenUseCaseImpl refreshAccessTokenUseCase;

    private RefreshAccessTokenCommand command;
    private static final String REFRESH_TOKEN = "refresh_token_xyz";
    private static final String HASHED_TOKEN = "hashed_refresh_token";
    private static final String NEW_ACCESS_TOKEN = "new_access_token_abc";
    private static final UUID TOKEN_ID = UUID.randomUUID();
    private static final UUID USER_ID = UUID.randomUUID();
    private static final String USER_EMAIL = "user@example.com";
    private static final String USERNAME = "testuser";
    private static final String HASHED_PASSWORD = "$2a$10$hashedPassword";

    @BeforeEach
    void setUp() {
        command = new RefreshAccessTokenCommand(REFRESH_TOKEN);
    }

    @Test
    void shouldRefreshAccessTokenSuccessfully() {
        // Given
        RefreshToken refreshToken = createRefreshToken(TOKEN_ID, USER_ID, HASHED_TOKEN, false, LocalDateTime.now().plusDays(7));
        SecureUserInfoApiDto user = new SecureUserInfoApiDto(USER_ID, USER_EMAIL, USERNAME, HASHED_PASSWORD, Instant.now(),true);

        when(tokenService.hashToken(REFRESH_TOKEN)).thenReturn(HASHED_TOKEN);
        when(refreshTokenRepository.findByToken(HASHED_TOKEN)).thenReturn(Optional.of(refreshToken));
        when(userApi.getSecureUserDataById(USER_ID)).thenReturn(Optional.of(user));
        when(tokenService.generateAccessToken(USER_ID, USER_EMAIL)).thenReturn(NEW_ACCESS_TOKEN);

        // When
        AuthTokens result = refreshAccessTokenUseCase.executeInternal(command);

        // Then
        assertThat(result.accessToken()).isEqualTo(NEW_ACCESS_TOKEN);
        assertThat(result.refreshToken()).isEqualTo(REFRESH_TOKEN);
        verify(tokenService).hashToken(REFRESH_TOKEN);
        verify(refreshTokenRepository).findByToken(HASHED_TOKEN);
        verify(userApi).getSecureUserDataById(USER_ID);
        verify(tokenService).generateAccessToken(USER_ID, USER_EMAIL);
    }

    @Test
    void shouldThrowInvalidRefreshTokenExceptionWhenTokenNotFound() {
        // Given
        when(tokenService.hashToken(REFRESH_TOKEN)).thenReturn(HASHED_TOKEN);
        when(refreshTokenRepository.findByToken(HASHED_TOKEN)).thenReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> refreshAccessTokenUseCase.executeInternal(command))
                .isInstanceOf(InvalidRefreshTokenException.class)
                .hasMessage("Refresh token not found");

        verify(tokenService).hashToken(REFRESH_TOKEN);
        verify(refreshTokenRepository).findByToken(HASHED_TOKEN);
        verify(userApi, never()).getSecureUserDataById(any());
        verify(tokenService, never()).generateAccessToken(any(), anyString());
    }

    @Test
    void shouldThrowRefreshTokenExpiredExceptionWhenTokenIsRevoked() {
        // Given
        RefreshToken refreshToken = createRefreshToken(TOKEN_ID, USER_ID, HASHED_TOKEN, true, LocalDateTime.now().plusDays(7));

        when(tokenService.hashToken(REFRESH_TOKEN)).thenReturn(HASHED_TOKEN);
        when(refreshTokenRepository.findByToken(HASHED_TOKEN)).thenReturn(Optional.of(refreshToken));

        // When & Then
        assertThatThrownBy(() -> refreshAccessTokenUseCase.executeInternal(command))
                .isInstanceOf(RefreshTokenExpiredException.class)
                .hasMessage("Refresh token has expired");

        verify(tokenService).hashToken(REFRESH_TOKEN);
        verify(refreshTokenRepository).findByToken(HASHED_TOKEN);
        verify(userApi, never()).getSecureUserDataById(any());
        verify(tokenService, never()).generateAccessToken(any(), anyString());
    }

    @Test
    void shouldThrowRefreshTokenExpiredExceptionWhenTokenExpired() {
        // Given
        RefreshToken refreshToken = createRefreshToken(TOKEN_ID, USER_ID, HASHED_TOKEN, false, LocalDateTime.now().minusDays(1));

        when(tokenService.hashToken(REFRESH_TOKEN)).thenReturn(HASHED_TOKEN);
        when(refreshTokenRepository.findByToken(HASHED_TOKEN)).thenReturn(Optional.of(refreshToken));

        // When & Then
        assertThatThrownBy(() -> refreshAccessTokenUseCase.executeInternal(command))
                .isInstanceOf(RefreshTokenExpiredException.class)
                .hasMessage("Refresh token has expired");

        verify(tokenService).hashToken(REFRESH_TOKEN);
        verify(refreshTokenRepository).findByToken(HASHED_TOKEN);
        verify(userApi, never()).getSecureUserDataById(any());
        verify(tokenService, never()).generateAccessToken(any(), anyString());
    }

    @Test
    void shouldThrowUserNotFoundExceptionWhenUserDoesNotExist() {
        // Given
        RefreshToken refreshToken = createRefreshToken(TOKEN_ID, USER_ID, HASHED_TOKEN, false, LocalDateTime.now().plusDays(7));

        when(tokenService.hashToken(REFRESH_TOKEN)).thenReturn(HASHED_TOKEN);
        when(refreshTokenRepository.findByToken(HASHED_TOKEN)).thenReturn(Optional.of(refreshToken));
        when(userApi.getSecureUserDataById(USER_ID)).thenReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> refreshAccessTokenUseCase.executeInternal(command))
                .isInstanceOf(UserNotFoundException.class)
                .hasMessage("User not found");

        verify(tokenService).hashToken(REFRESH_TOKEN);
        verify(refreshTokenRepository).findByToken(HASHED_TOKEN);
        verify(userApi).getSecureUserDataById(USER_ID);
        verify(tokenService, never()).generateAccessToken(any(), anyString());
    }

    @Test
    void shouldThrowIllegalArgumentExceptionWhenRefreshTokenIsNull() {
        // Given
        RefreshAccessTokenCommand invalidCommand = new RefreshAccessTokenCommand(null);

        // When & Then
        assertThatThrownBy(() -> {
            invalidCommand.validate();
            refreshAccessTokenUseCase.executeInternal(invalidCommand);
        })
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Refresh token cannot be blank");

        verify(tokenService, never()).hashToken(anyString());
        verify(refreshTokenRepository, never()).findByToken(anyString());
        verify(userApi, never()).getSecureUserDataById(any());
    }

    @Test
    void shouldThrowIllegalArgumentExceptionWhenRefreshTokenIsBlank() {
        // Given
        RefreshAccessTokenCommand invalidCommand = new RefreshAccessTokenCommand("   ");

        // When & Then
        assertThatThrownBy(() -> {
            invalidCommand.validate();
            refreshAccessTokenUseCase.executeInternal(invalidCommand);
        })
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Refresh token cannot be blank");

        verify(tokenService, never()).hashToken(anyString());
        verify(refreshTokenRepository, never()).findByToken(anyString());
        verify(userApi, never()).getSecureUserDataById(any());
    }

    private RefreshToken createRefreshToken(UUID id, UUID userId, String token, boolean isRevoked, LocalDateTime expiresAt) {
        RefreshToken refreshToken = new RefreshToken();
        refreshToken.setId(id);
        refreshToken.setUserId(userId);
        refreshToken.setToken(token);
        refreshToken.setRevoked(isRevoked);
        refreshToken.setExpiresAt(expiresAt);
        return refreshToken;
    }
}
package unittests;

import edu.pjwstk.auth.exception.domain.InvalidRefreshTokenException;
import edu.pjwstk.auth.models.RefreshToken;
import edu.pjwstk.auth.repository.JpaRefreshTokenRepository;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.logout.LogoutUserCommand;
import edu.pjwstk.auth.usecase.logout.LogoutUserUseCaseImpl;
import jakarta.validation.ValidationException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class LogoutUserUseCaseTest {

    @Mock
    private TokenService tokenService;

    @Mock
    private JpaRefreshTokenRepository refreshTokenRepository;

    @InjectMocks
    private LogoutUserUseCaseImpl logoutUserUseCase;

    private LogoutUserCommand command;
    private static final String REFRESH_TOKEN = "refresh_token_xyz";
    private static final String HASHED_TOKEN = "hashed_refresh_token";
    private static final UUID TOKEN_ID = UUID.randomUUID();
    private static final UUID USER_ID = UUID.randomUUID();

    @BeforeEach
    void setUp() {
        command = new LogoutUserCommand(REFRESH_TOKEN);
    }

    @Test
    void shouldLogoutUserSuccessfully() {
        // Given
        RefreshToken refreshToken = createRefreshToken(TOKEN_ID, USER_ID, HASHED_TOKEN, false, LocalDateTime.now().plusDays(7));

        when(tokenService.hashToken(REFRESH_TOKEN)).thenReturn(HASHED_TOKEN);
        when(refreshTokenRepository.findByToken(HASHED_TOKEN)).thenReturn(Optional.of(refreshToken));
        doNothing().when(refreshTokenRepository).updateRevokedById(TOKEN_ID, true);

        // When
        Void result = logoutUserUseCase.executeInternal(command);

        // Then
        assertThat(result).isNull();
        verify(tokenService).hashToken(REFRESH_TOKEN);
        verify(refreshTokenRepository).findByToken(HASHED_TOKEN);
        verify(refreshTokenRepository).updateRevokedById(TOKEN_ID, true);
    }

    @Test
    void shouldThrowInvalidRefreshTokenExceptionWhenTokenNotFound() {
        // Given
        when(tokenService.hashToken(REFRESH_TOKEN)).thenReturn(HASHED_TOKEN);
        when(refreshTokenRepository.findByToken(HASHED_TOKEN)).thenReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> logoutUserUseCase.executeInternal(command))
                .isInstanceOf(InvalidRefreshTokenException.class)
                .hasMessage("Refresh token not found");

        verify(tokenService).hashToken(REFRESH_TOKEN);
        verify(refreshTokenRepository).findByToken(HASHED_TOKEN);
        verify(refreshTokenRepository, never()).updateRevokedById(any(), anyBoolean());
    }

    @Test
    void shouldThrowValidationExceptionWhenRefreshTokenIsNull() {
        // Given
        LogoutUserCommand invalidCommand = new LogoutUserCommand(null);

        // When & Then
        assertThatThrownBy(() -> {
            invalidCommand.validate();
            logoutUserUseCase.executeInternal(invalidCommand);
        })
                .isInstanceOf(ValidationException.class)
                .hasMessage("Refresh token cannot be blank");

        verify(tokenService, never()).hashToken(anyString());
        verify(refreshTokenRepository, never()).findByToken(anyString());
        verify(refreshTokenRepository, never()).updateRevokedById(any(), anyBoolean());
    }

    @Test
    void shouldThrowValidationExceptionWhenRefreshTokenIsBlank() {
        // Given
        LogoutUserCommand invalidCommand = new LogoutUserCommand("   ");

        // When & Then
        assertThatThrownBy(() -> {
            invalidCommand.validate();
            logoutUserUseCase.executeInternal(invalidCommand);
        })
                .isInstanceOf(ValidationException.class)
                .hasMessage("Refresh token cannot be blank");

        verify(tokenService, never()).hashToken(anyString());
        verify(refreshTokenRepository, never()).findByToken(anyString());
        verify(refreshTokenRepository, never()).updateRevokedById(any(), anyBoolean());
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
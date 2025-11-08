package unittests;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.auth.exception.domain.InvalidCredentialsException;
import edu.pjwstk.auth.service.EmailVerificationService;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.common.LoginUserResult;
import edu.pjwstk.auth.usecase.login.LoginUserCommand;
import edu.pjwstk.auth.usecase.login.LoginUserUseCaseImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.time.Instant;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class LoginUserUseCaseTest {

    @Mock
    private UserApi userApi;

    @Mock
    private PasswordEncoder passwordEncoder;

    @Mock
    private TokenService tokenService;

    @Mock
    private EmailVerificationService emailVerificationService;

    @InjectMocks
    private LoginUserUseCaseImpl loginUserUseCase;

    private LoginUserCommand command;
    private static final String USER_EMAIL = "user@example.com";
    private static final String USER_PASSWORD = "Password123!";
    private static final String HASHED_PASSWORD = "$2a$10$hashedPassword";
    private static final UUID USER_ID = UUID.randomUUID();
    private static final String USERNAME = "testuser";
    private static final String ACCESS_TOKEN = "access_token_xyz";
    private static final String REFRESH_TOKEN = "refresh_token_xyz";
    private static final String VERIFICATION_CODE = "123456";

    @BeforeEach
    void setUp() {
        command = new LoginUserCommand(USER_EMAIL, USER_PASSWORD);
    }

    @Test
    void shouldLoginUserSuccessfullyWhenEmailIsVerified() {
        // Given
        SecureUserInfoApiDto user = new SecureUserInfoApiDto(
                USER_ID,
                USER_EMAIL,
                USERNAME,
                HASHED_PASSWORD,
                Instant.now(),
                true
        );
        AuthTokens authTokens = new AuthTokens(ACCESS_TOKEN, REFRESH_TOKEN);

        when(userApi.getSecureUserDataByEmail(USER_EMAIL)).thenReturn(Optional.of(user));
        when(passwordEncoder.matches(USER_PASSWORD, HASHED_PASSWORD)).thenReturn(true);
        when(tokenService.generateTokenPair(USER_ID, USER_EMAIL, true)).thenReturn(authTokens);

        // When
        LoginUserResult result = loginUserUseCase.executeInternal(command);

        // Then
        assertThat(result.userId()).isEqualTo(USER_ID);
        assertThat(result.email()).isEqualTo(USER_EMAIL);
        assertThat(result.username()).isEqualTo(USERNAME);
        assertThat(result.isEmailVerified()).isTrue();
        assertThat(result.authTokens()).isEqualTo(authTokens);

        verify(userApi).getSecureUserDataByEmail(USER_EMAIL);
        verify(passwordEncoder).matches(USER_PASSWORD, HASHED_PASSWORD);
        verify(tokenService).generateTokenPair(USER_ID, USER_EMAIL, true);
        verify(emailVerificationService, never()).generateAndSaveEmailVerificationCode(any());
        verify(emailVerificationService, never()).sendEmailVerificationCode(any(), anyString(), anyString());
    }


    @Test
    void shouldThrowInvalidCredentialsExceptionWhenUserNotFound() {
        // Given
        when(userApi.getSecureUserDataByEmail(USER_EMAIL)).thenReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> loginUserUseCase.executeInternal(command))
                .isInstanceOf(InvalidCredentialsException.class)
                .hasMessage("Login credentials are invalid");

        verify(userApi).getSecureUserDataByEmail(USER_EMAIL);
        verify(passwordEncoder, never()).matches(anyString(), anyString());
        verify(tokenService, never()).generateTokenPair(any(), anyString(), anyBoolean());
        verify(emailVerificationService, never()).generateAndSaveEmailVerificationCode(any());
    }

    @Test
    void shouldThrowInvalidCredentialsExceptionWhenPasswordDoesNotMatch() {
        // Given
        SecureUserInfoApiDto user = new SecureUserInfoApiDto(
                USER_ID,
                USER_EMAIL,
                USERNAME,
                HASHED_PASSWORD,
                Instant.now(),
                true
        );

        when(userApi.getSecureUserDataByEmail(USER_EMAIL)).thenReturn(Optional.of(user));
        when(passwordEncoder.matches(USER_PASSWORD, HASHED_PASSWORD)).thenReturn(false);

        // When & Then
        assertThatThrownBy(() -> loginUserUseCase.executeInternal(command))
                .isInstanceOf(InvalidCredentialsException.class)
                .hasMessage("Login credentials are invalid");

        verify(userApi).getSecureUserDataByEmail(USER_EMAIL);
        verify(passwordEncoder).matches(USER_PASSWORD, HASHED_PASSWORD);
        verify(tokenService, never()).generateTokenPair(any(), anyString(), anyBoolean());
        verify(emailVerificationService, never()).generateAndSaveEmailVerificationCode(any());
    }
}
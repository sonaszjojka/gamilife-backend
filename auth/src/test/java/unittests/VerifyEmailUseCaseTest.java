package unittests;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.auth.exception.domain.EmailVerificationCodeExpiredException;
import edu.pjwstk.auth.exception.domain.InvalidEmailVerificationCodeException;
import edu.pjwstk.auth.models.EmailVerificationCode;
import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.service.EmailVerificationService;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.common.LoginUserResult;
import edu.pjwstk.auth.usecase.verifyemail.VerifyEmailCommand;
import edu.pjwstk.auth.usecase.verifyemail.VerifyEmailUseCaseImpl;
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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class VerifyEmailUseCaseTest {

    @Mock
    private JpaEmailVerificationRepository emailVerificationRepository;

    @Mock
    private EmailVerificationService emailVerificationService;

    @Mock
    private TokenService tokenService;

    @Mock
    private UserApi userApi;

    @InjectMocks
    private VerifyEmailUseCaseImpl verifyEmailUseCase;

    private VerifyEmailCommand command;
    private static final UUID USER_ID = UUID.randomUUID();
    private static final String CODE = "123456";
    private static final String HASHED_CODE = "hashed_code_123";
    private static final String USER_EMAIL = "user@example.com";
    private static final String USERNAME = "testuser";
    private static final String ACCESS_TOKEN = "access_token_xyz";
    private static final String REFRESH_TOKEN = "refresh_token_xyz";

    @BeforeEach
    void setUp() {
        command = new VerifyEmailCommand(USER_ID, CODE);
    }

    @Test
    void shouldVerifyEmailSuccessfully() {
        // Given
        EmailVerificationCode emailVerificationCode = createEmailVerificationCode(
                USER_ID, HASHED_CODE, false, LocalDateTime.now().plusMinutes(15));
        BasicUserInfoApiDto user = new BasicUserInfoApiDto(USER_ID, USER_EMAIL, USERNAME);
        AuthTokens authTokens = new AuthTokens(ACCESS_TOKEN, REFRESH_TOKEN);

        when(emailVerificationService.hashCode(CODE)).thenReturn(HASHED_CODE);
        when(emailVerificationRepository.findByUserIdAndCode(USER_ID, HASHED_CODE))
                .thenReturn(Optional.of(emailVerificationCode));
        when(userApi.confirmUserEmailVerification(USER_ID)).thenReturn(user);
        when(tokenService.generateTokenPair(USER_ID, USER_EMAIL, true)).thenReturn(authTokens);

        // When
        LoginUserResult result = verifyEmailUseCase.executeInternal(command);

        // Then
        assertThat(result.userId()).isEqualTo(USER_ID);
        assertThat(result.email()).isEqualTo(USER_EMAIL);
        assertThat(result.username()).isEqualTo(user.username());
        assertThat(result.isEmailVerified()).isTrue();
        assertThat(result.authTokens()).isEqualTo(authTokens);

        verify(emailVerificationService).hashCode(CODE);
        verify(emailVerificationRepository).findByUserIdAndCode(USER_ID, HASHED_CODE);
        verify(userApi).confirmUserEmailVerification(USER_ID);
        verify(tokenService).generateTokenPair(USER_ID, USER_EMAIL, true);
    }

    @Test
    void shouldThrowInvalidEmailVerificationCodeExceptionWhenCodeNotFound() {
        // Given
        when(emailVerificationService.hashCode(CODE)).thenReturn(HASHED_CODE);
        when(emailVerificationRepository.findByUserIdAndCode(USER_ID, HASHED_CODE))
                .thenReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> verifyEmailUseCase.executeInternal(command))
                .isInstanceOf(InvalidEmailVerificationCodeException.class)
                .hasMessage("Invalid verification code.");

        verify(emailVerificationService).hashCode(CODE);
        verify(emailVerificationRepository).findByUserIdAndCode(USER_ID, HASHED_CODE);
        verify(userApi, never()).confirmUserEmailVerification(any());
        verify(tokenService, never()).generateTokenPair(any(), anyString(), anyBoolean());
    }

    @Test
    void shouldThrowEmailVerificationCodeExpiredExceptionWhenCodeIsRevoked() {
        // Given
        EmailVerificationCode emailVerificationCode = createEmailVerificationCode(
                USER_ID, HASHED_CODE, true, LocalDateTime.now().plusMinutes(15));

        when(emailVerificationService.hashCode(CODE)).thenReturn(HASHED_CODE);
        when(emailVerificationRepository.findByUserIdAndCode(USER_ID, HASHED_CODE))
                .thenReturn(Optional.of(emailVerificationCode));

        // When & Then
        assertThatThrownBy(() -> verifyEmailUseCase.executeInternal(command))
                .isInstanceOf(EmailVerificationCodeExpiredException.class)
                .hasMessage("Email verification code has expired.");

        verify(emailVerificationService).hashCode(CODE);
        verify(emailVerificationRepository).findByUserIdAndCode(USER_ID, HASHED_CODE);
        verify(userApi, never()).confirmUserEmailVerification(any());
        verify(tokenService, never()).generateTokenPair(any(), anyString(), anyBoolean());
    }

    @Test
    void shouldThrowEmailVerificationCodeExpiredExceptionWhenCodeExpired() {
        // Given
        EmailVerificationCode emailVerificationCode = createEmailVerificationCode(
                USER_ID, HASHED_CODE, false, LocalDateTime.now().minusMinutes(1));

        when(emailVerificationService.hashCode(CODE)).thenReturn(HASHED_CODE);
        when(emailVerificationRepository.findByUserIdAndCode(USER_ID, HASHED_CODE))
                .thenReturn(Optional.of(emailVerificationCode));

        // When & Then
        assertThatThrownBy(() -> verifyEmailUseCase.executeInternal(command))
                .isInstanceOf(EmailVerificationCodeExpiredException.class)
                .hasMessage("Email verification code has expired.");

        verify(emailVerificationService).hashCode(CODE);
        verify(emailVerificationRepository).findByUserIdAndCode(USER_ID, HASHED_CODE);
        verify(userApi, never()).confirmUserEmailVerification(any());
        verify(tokenService, never()).generateTokenPair(any(), anyString(), anyBoolean());
    }

    @Test
    void shouldThrowIllegalArgumentExceptionWhenUserIdIsNull() {
        // Given
        VerifyEmailCommand invalidCommand = new VerifyEmailCommand(null, CODE);

        // When & Then
        assertThatThrownBy(() -> {
            invalidCommand.validate();
            verifyEmailUseCase.executeInternal(invalidCommand);
        })
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("User id cannot be null");

        verify(emailVerificationService, never()).hashCode(anyString());
        verify(emailVerificationRepository, never()).findByUserIdAndCode(any(), anyString());
        verify(userApi, never()).confirmUserEmailVerification(any());
    }

    @Test
    void shouldThrowIllegalArgumentExceptionWhenCodeIsNull() {
        // Given
        VerifyEmailCommand invalidCommand = new VerifyEmailCommand(USER_ID, null);

        // When & Then
        assertThatThrownBy(() -> {
            invalidCommand.validate();
            verifyEmailUseCase.executeInternal(invalidCommand);
        })
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Code cannot be null or blank");

        verify(emailVerificationService, never()).hashCode(anyString());
        verify(emailVerificationRepository, never()).findByUserIdAndCode(any(), anyString());
        verify(userApi, never()).confirmUserEmailVerification(any());
    }

    @Test
    void shouldThrowIllegalArgumentExceptionWhenCodeIsBlank() {
        // Given
        VerifyEmailCommand invalidCommand = new VerifyEmailCommand(USER_ID, "   ");

        // When & Then
        assertThatThrownBy(() -> {
            invalidCommand.validate();
            verifyEmailUseCase.executeInternal(invalidCommand);
        })
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Code cannot be null or blank");

        verify(emailVerificationService, never()).hashCode(anyString());
        verify(emailVerificationRepository, never()).findByUserIdAndCode(any(), anyString());
        verify(userApi, never()).confirmUserEmailVerification(any());
    }

    private EmailVerificationCode createEmailVerificationCode(UUID userId, String code, boolean isRevoked, LocalDateTime expiresAt) {
        EmailVerificationCode verificationCode = new EmailVerificationCode();
        verificationCode.setId(UUID.randomUUID());
        verificationCode.setUserId(userId);
        verificationCode.setCode(code);
        verificationCode.setRevoked(isRevoked);
        verificationCode.setIssuedAt(LocalDateTime.now());
        verificationCode.setExpiresAt(expiresAt);
        return verificationCode;
    }
}
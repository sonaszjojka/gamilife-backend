package unittests;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.auth.exception.domain.InvalidCredentialsException;
import edu.pjwstk.auth.exception.domain.LinkedUserNotFoundException;
import edu.pjwstk.auth.exception.domain.UserAlreadyLinkedToProviderException;
import edu.pjwstk.auth.models.UserOAuthProvider;
import edu.pjwstk.auth.repository.JpaUserProviderRepository;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.common.LoginUserResult;
import edu.pjwstk.auth.usecase.googlelinkaccount.LinkGoogleAccountCommand;
import edu.pjwstk.auth.usecase.googlelinkaccount.LinkGoogleAccountUseCaseImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.time.Instant;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class LinkGoogleAccountUseCaseTest {

    @Mock
    private UserApi userApi;

    @Mock
    private PasswordEncoder passwordEncoder;

    @Mock
    private JpaUserProviderRepository userProviderRepository;

    @Mock
    private TokenService tokenService;

    @InjectMocks
    private LinkGoogleAccountUseCaseImpl linkGoogleAccountUseCase;

    private LinkGoogleAccountCommand command;
    private SecureUserInfoApiDto secureUserInfo;
    private AuthTokens authTokens;

    private static final UUID USER_ID = UUID.randomUUID();
    private static final String EMAIL = "user@example.com";
    private static final String USERNAME = "testuser";
    private static final String PASSWORD = "Password123!";
    private static final String HASHED_PASSWORD = "$2a$10$hashedPassword";
    private static final String PROVIDER = "google";
    private static final String PROVIDER_ID = "google123456";
    private static final String ACCESS_TOKEN = "access_token_123";
    private static final String REFRESH_TOKEN = "refresh_token_456";

    @BeforeEach
    void setUp() {
        command = new LinkGoogleAccountCommand(
                true,
                PROVIDER,
                PROVIDER_ID,
                USER_ID,
                PASSWORD
        );

        secureUserInfo = new SecureUserInfoApiDto(
                USER_ID,
                EMAIL,
                USERNAME,
                HASHED_PASSWORD,
                Instant.from(Instant.now()),
                true
        );

        authTokens = new AuthTokens(ACCESS_TOKEN, REFRESH_TOKEN);
    }

    @Test
    void shouldLinkGoogleAccountSuccessfully() {
        // Given
        when(userApi.getSecureUserDataById(USER_ID)).thenReturn(Optional.of(secureUserInfo));
        when(passwordEncoder.matches(PASSWORD, HASHED_PASSWORD)).thenReturn(true);
        when(userProviderRepository.existsByUserIdAndProvider(USER_ID, PROVIDER)).thenReturn(false);
        when(tokenService.generateTokenPair(USER_ID, EMAIL, true)).thenReturn(authTokens);

        // When
        Optional<LoginUserResult> result = linkGoogleAccountUseCase.executeInternal(command);

        // Then
        assertThat(result).isPresent();
        LoginUserResult loginResult = result.get();
        assertThat(loginResult.userId()).isEqualTo(USER_ID);
        assertThat(loginResult.email()).isEqualTo(EMAIL);
        assertThat(loginResult.username()).isEqualTo(USERNAME);
        assertThat(loginResult.isEmailVerified()).isTrue();
        assertThat(loginResult.authTokens()).isEqualTo(authTokens);

        verify(userApi).getSecureUserDataById(USER_ID);
        verify(passwordEncoder).matches(PASSWORD, HASHED_PASSWORD);
        verify(userProviderRepository).existsByUserIdAndProvider(USER_ID, PROVIDER);

        ArgumentCaptor<UserOAuthProvider> providerCaptor = ArgumentCaptor.forClass(UserOAuthProvider.class);
        verify(userProviderRepository).save(providerCaptor.capture());
        UserOAuthProvider savedProvider = providerCaptor.getValue();
        assertThat(savedProvider.getUserId()).isEqualTo(USER_ID);
        assertThat(savedProvider.getProvider()).isEqualTo(PROVIDER);
        assertThat(savedProvider.getProviderId()).isEqualTo(PROVIDER_ID);

        verify(userApi, never()).confirmUserEmailVerification(any());
        verify(tokenService).generateTokenPair(USER_ID, EMAIL, true);
    }

    @Test
    void shouldThrowLinkedUserNotFoundExceptionWhenUserNotFound() {
        // Given
        when(userApi.getSecureUserDataById(USER_ID)).thenReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> linkGoogleAccountUseCase.executeInternal(command))
                .isInstanceOf(LinkedUserNotFoundException.class)
                .hasMessage("Local user to link to not found");

        verify(userApi).getSecureUserDataById(USER_ID);
        verify(passwordEncoder, never()).matches(anyString(), anyString());
        verify(userProviderRepository, never()).existsByUserIdAndProvider(any(), anyString());
        verify(userProviderRepository, never()).save(any());
        verify(tokenService, never()).generateTokenPair(any(), anyString(), anyBoolean());
    }

    @Test
    void shouldThrowInvalidCredentialsExceptionWhenPasswordDoesNotMatch() {
        // Given
        when(userApi.getSecureUserDataById(USER_ID)).thenReturn(Optional.of(secureUserInfo));
        when(passwordEncoder.matches(PASSWORD, HASHED_PASSWORD)).thenReturn(false);

        // When & Then
        assertThatThrownBy(() -> linkGoogleAccountUseCase.executeInternal(command))
                .isInstanceOf(InvalidCredentialsException.class)
                .hasMessage("Invalid password");

        verify(userApi).getSecureUserDataById(USER_ID);
        verify(passwordEncoder).matches(PASSWORD, HASHED_PASSWORD);
        verify(userProviderRepository, never()).existsByUserIdAndProvider(any(), anyString());
        verify(userProviderRepository, never()).save(any());
        verify(tokenService, never()).generateTokenPair(any(), anyString(), anyBoolean());
    }

    @Test
    void shouldThrowUserAlreadyLinkedToProviderExceptionWhenAlreadyLinked() {
        // Given
        when(userApi.getSecureUserDataById(USER_ID)).thenReturn(Optional.of(secureUserInfo));
        when(passwordEncoder.matches(PASSWORD, HASHED_PASSWORD)).thenReturn(true);
        when(userProviderRepository.existsByUserIdAndProvider(USER_ID, PROVIDER)).thenReturn(true);

        // When & Then
        assertThatThrownBy(() -> linkGoogleAccountUseCase.executeInternal(command))
                .isInstanceOf(UserAlreadyLinkedToProviderException.class)
                .hasMessage("User is already linked to this provider");

        verify(userApi).getSecureUserDataById(USER_ID);
        verify(passwordEncoder).matches(PASSWORD, HASHED_PASSWORD);
        verify(userProviderRepository).existsByUserIdAndProvider(USER_ID, PROVIDER);
        verify(userProviderRepository, never()).save(any());
        verify(tokenService, never()).generateTokenPair(any(), anyString(), anyBoolean());
    }
}
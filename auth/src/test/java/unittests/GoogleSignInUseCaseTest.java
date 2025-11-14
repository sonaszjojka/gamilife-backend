package unittests;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.auth.dto.GoogleUserDto;
import edu.pjwstk.auth.models.UserOAuthProvider;
import edu.pjwstk.auth.repository.JpaUserProviderRepository;
import edu.pjwstk.auth.service.OAuthService;
import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.auth.usecase.common.LoginUserResult;
import edu.pjwstk.auth.usecase.googlesignin.GoogleSignInCommand;
import edu.pjwstk.auth.usecase.googlesignin.GoogleSignInResult;
import edu.pjwstk.auth.usecase.googlesignin.GoogleSignUseCaseImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class GoogleSignInUseCaseTest {

    @Mock
    private OAuthService oAuthService;

    @Mock
    private JpaUserProviderRepository userProviderRepository;

    @Mock
    private UserApi userApi;

    @InjectMocks
    private GoogleSignUseCaseImpl googleSignInUseCase;

    private GoogleSignInCommand command;
    private static final String CODE = "google_auth_code_123";
    private static final String CODE_VERIFIER = "code_verifier_xyz";
    private static final String ID_TOKEN = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...";
    private static final String GOOGLE_SUB = "google_user_123456";
    private static final String USER_EMAIL = "user@example.com";
    private static final UUID USER_ID = UUID.randomUUID();

    @BeforeEach
    void setUp() {
        command = new GoogleSignInCommand(CODE, CODE_VERIFIER);
    }

    @Test
    void shouldSignInExistingOAuthUserSuccessfully() {
        // Given
        Map<String, String> tokenResponse = Map.of("id_token", ID_TOKEN);
        GoogleUserDto googleUserDto = new GoogleUserDto(GOOGLE_SUB, USER_EMAIL, "John", "Doe");
        UserOAuthProvider existingOAuthUser = createUserOAuthProvider(USER_ID, "google", GOOGLE_SUB);
        AuthTokens authTokens = new AuthTokens("access_token", "refresh_token");
        LoginUserResult loginUserResult = new LoginUserResult(USER_ID, USER_EMAIL, "john_doe", true, authTokens);
        GoogleSignInResult expectedResult = new GoogleSignInResult(GoogleSignInResult.LoginType.EXISTING_USER, loginUserResult);

        when(oAuthService.exchangeCodeForTokens(CODE, CODE_VERIFIER)).thenReturn(tokenResponse);
        when(oAuthService.extractUserInfoFromIdToken(ID_TOKEN)).thenReturn(googleUserDto);
        when(userProviderRepository.findByProviderAndProviderId("google", GOOGLE_SUB))
                .thenReturn(Optional.of(existingOAuthUser));
        when(oAuthService.loginViaGoogle(USER_ID, USER_EMAIL)).thenReturn(expectedResult);

        // When
        GoogleSignInResult result = googleSignInUseCase.executeInternal(command);

        // Then
        assertThat(result).isEqualTo(expectedResult);
        assertThat(result.getLoginType()).isEqualTo(GoogleSignInResult.LoginType.EXISTING_USER);
        verify(oAuthService).exchangeCodeForTokens(CODE, CODE_VERIFIER);
        verify(oAuthService).extractUserInfoFromIdToken(ID_TOKEN);
        verify(userProviderRepository).findByProviderAndProviderId("google", GOOGLE_SUB);
        verify(oAuthService).loginViaGoogle(USER_ID, USER_EMAIL);
        verify(userApi, never()).getUserByEmail(anyString());
        verify(oAuthService, never()).registerViaGoogle(any());
    }

    @Test
    void shouldThrowExceptionWhenCodeIsNull() {
        // Given
        GoogleSignInCommand invalidCommand = new GoogleSignInCommand(null, CODE_VERIFIER);

        // When & Then
        assertThatThrownBy(() -> googleSignInUseCase.executeInternal(invalidCommand))
                .isInstanceOf(NullPointerException.class);

        verify(oAuthService, never()).exchangeCodeForTokens(anyString(), anyString());
        verify(oAuthService, never()).extractUserInfoFromIdToken(anyString());
        verify(userProviderRepository, never()).findByProviderAndProviderId(anyString(), anyString());
    }

    @Test
    void shouldThrowExceptionWhenCodeVerifierIsNull() {
        // Given
        GoogleSignInCommand invalidCommand = new GoogleSignInCommand(CODE, null);

        // When & Then
        assertThatThrownBy(() -> googleSignInUseCase.executeInternal(invalidCommand))
                .isInstanceOf(NullPointerException.class);

        verify(oAuthService, never()).exchangeCodeForTokens(anyString(), anyString());
        verify(oAuthService, never()).extractUserInfoFromIdToken(anyString());
        verify(userProviderRepository, never()).findByProviderAndProviderId(anyString(), anyString());
    }

    @Test
    void shouldThrowExceptionWhenTokenExchangeFails() {
        // Given
        when(oAuthService.exchangeCodeForTokens(CODE, CODE_VERIFIER))
                .thenThrow(new RuntimeException("Failed to exchange code for tokens"));

        // When & Then
        assertThatThrownBy(() -> googleSignInUseCase.executeInternal(command))
                .isInstanceOf(RuntimeException.class)
                .hasMessage("Failed to exchange code for tokens");

        verify(oAuthService).exchangeCodeForTokens(CODE, CODE_VERIFIER);
        verify(oAuthService, never()).extractUserInfoFromIdToken(anyString());
        verify(userProviderRepository, never()).findByProviderAndProviderId(anyString(), anyString());
    }

    // Helper method
    private UserOAuthProvider createUserOAuthProvider(UUID userId, String provider, String providerId) {
        return new UserOAuthProvider(
                UUID.randomUUID(),
                userId,
                provider,
                providerId
        );
    }
}
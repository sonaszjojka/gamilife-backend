package unittests;

import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.auth.security.UserDetailsImpl;
import edu.pjwstk.auth.usecase.getauthuser.GetAuthenticatedUserCommand;
import edu.pjwstk.auth.usecase.getauthuser.GetAuthenticatedUserDataUseCaseImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class GetAuthenticatedUserDataUseCaseTest {

    @Mock
    private SecurityContext securityContext;

    @Mock
    private Authentication authentication;

    @Mock
    private UserDetailsImpl userDetails;

    @InjectMocks
    private GetAuthenticatedUserDataUseCaseImpl getAuthenticatedUserDataUseCase;

    private GetAuthenticatedUserCommand command;

    private static final UUID USER_ID = UUID.randomUUID();

    @BeforeEach
    void setUp() {
        command = new GetAuthenticatedUserCommand();
        SecurityContextHolder.setContext(securityContext);
    }

    @AfterEach
    void tearDown() {
        SecurityContextHolder.clearContext();
    }

    @Test
    void shouldReturnCurrentUserDataSuccessfully() {
        // Given
        when(securityContext.getAuthentication()).thenReturn(authentication);
        when(authentication.getPrincipal()).thenReturn(userDetails);
        when(userDetails.getId()).thenReturn(USER_ID);

        // When
        CurrentUserDto result = getAuthenticatedUserDataUseCase.executeInternal(command);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.userId()).isEqualTo(USER_ID);
    }

    @Test
    void shouldThrowRuntimeExceptionWhenPrincipalIsNotUserDetailsImpl() {
        // Given
        String anonymousPrincipal = "anonymousUser";
        when(securityContext.getAuthentication()).thenReturn(authentication);
        when(authentication.getPrincipal()).thenReturn(anonymousPrincipal);

        // When & Then
        assertThatThrownBy(() -> getAuthenticatedUserDataUseCase.executeInternal(command))
                .isInstanceOf(RuntimeException.class)
                .hasMessage("Access denied");
    }

    @Test
    void shouldThrowRuntimeExceptionWhenPrincipalIsNull() {
        // Given
        when(securityContext.getAuthentication()).thenReturn(authentication);
        when(authentication.getPrincipal()).thenReturn(null);

        // When & Then
        assertThatThrownBy(() -> getAuthenticatedUserDataUseCase.executeInternal(command))
                .isInstanceOf(RuntimeException.class)
                .hasMessage("Access denied");
    }
}
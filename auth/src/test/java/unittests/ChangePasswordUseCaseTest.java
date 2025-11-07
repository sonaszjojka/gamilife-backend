package unittests;

import edu.pjwstk.auth.exception.domain.InvalidCredentialsException;
import edu.pjwstk.auth.exception.domain.OldAndNewPasswordAreTheSameException;
import edu.pjwstk.auth.usecase.changepassword.ChangePasswordCommand;
import edu.pjwstk.auth.usecase.changepassword.ChangePasswordUseCaseImpl;
import edu.pjwstk.auth.validators.PasswordValidator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.crypto.password.PasswordEncoder;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ChangePasswordUseCaseTest {

    @Mock
    private PasswordValidator passwordValidator;

    @Mock
    private PasswordEncoder passwordEncoder;

    @InjectMocks
    private ChangePasswordUseCaseImpl changePasswordUseCase;

    private ChangePasswordCommand command;
    private static final String PROVIDED_PASSWORD = "OldPassword123!";
    private static final String HASHED_USER_PASSWORD = "$2a$10$hashedOldPassword";
    private static final String NEW_PASSWORD = "NewPassword123!";
    private static final String ENCODED_NEW_PASSWORD = "$2a$10$hashedNewPassword";

    @BeforeEach
    void setUp() {
        command = new ChangePasswordCommand(
                PROVIDED_PASSWORD,
                HASHED_USER_PASSWORD,
                NEW_PASSWORD
        );
    }

    @Test
    void shouldChangePasswordSuccessfully() {
        // Given
        doNothing().when(passwordValidator).validate(NEW_PASSWORD);
        when(passwordEncoder.matches(PROVIDED_PASSWORD, HASHED_USER_PASSWORD)).thenReturn(true);
        when(passwordEncoder.matches(NEW_PASSWORD, HASHED_USER_PASSWORD)).thenReturn(false);
        when(passwordEncoder.encode(NEW_PASSWORD)).thenReturn(ENCODED_NEW_PASSWORD);

        // When
        String result = changePasswordUseCase.executeInternal(command);

        // Then
        assertThat(result).isEqualTo(ENCODED_NEW_PASSWORD);
        verify(passwordValidator).validate(NEW_PASSWORD);
        verify(passwordEncoder).matches(PROVIDED_PASSWORD, HASHED_USER_PASSWORD);
        verify(passwordEncoder).matches(NEW_PASSWORD, HASHED_USER_PASSWORD);
        verify(passwordEncoder).encode(NEW_PASSWORD);
    }

    @Test
    void shouldThrowInvalidCredentialsExceptionWhenProvidedPasswordDoesNotMatch() {
        // Given
        doNothing().when(passwordValidator).validate(NEW_PASSWORD);
        when(passwordEncoder.matches(PROVIDED_PASSWORD, HASHED_USER_PASSWORD)).thenReturn(false);

        // When & Then
        assertThatThrownBy(() -> changePasswordUseCase.executeInternal(command))
                .isInstanceOf(InvalidCredentialsException.class)
                .hasMessage("Invalid password");

        verify(passwordValidator).validate(NEW_PASSWORD);
        verify(passwordEncoder).matches(PROVIDED_PASSWORD, HASHED_USER_PASSWORD);
        verify(passwordEncoder, never()).matches(NEW_PASSWORD, HASHED_USER_PASSWORD);
        verify(passwordEncoder, never()).encode(anyString());
    }

    @Test
    void shouldThrowOldAndNewPasswordAreTheSameExceptionWhenPasswordsMatch() {
        // Given
        doNothing().when(passwordValidator).validate(NEW_PASSWORD);
        when(passwordEncoder.matches(PROVIDED_PASSWORD, HASHED_USER_PASSWORD)).thenReturn(true);
        when(passwordEncoder.matches(NEW_PASSWORD, HASHED_USER_PASSWORD)).thenReturn(true);

        // When & Then
        assertThatThrownBy(() -> changePasswordUseCase.executeInternal(command))
                .isInstanceOf(OldAndNewPasswordAreTheSameException.class);

        verify(passwordValidator).validate(NEW_PASSWORD);
        verify(passwordEncoder).matches(PROVIDED_PASSWORD, HASHED_USER_PASSWORD);
        verify(passwordEncoder).matches(NEW_PASSWORD, HASHED_USER_PASSWORD);
        verify(passwordEncoder, never()).encode(anyString());
    }

    @Test
    void shouldThrowExceptionWhenPasswordValidationFails() {
        // Given
        String invalidPassword = "weak";
        ChangePasswordCommand invalidCommand = new ChangePasswordCommand(
                PROVIDED_PASSWORD,
                HASHED_USER_PASSWORD,
                invalidPassword
        );
        doThrow(new IllegalArgumentException("Password does not meet requirements"))
                .when(passwordValidator).validate(invalidPassword);

        // When & Then
        assertThatThrownBy(() -> changePasswordUseCase.executeInternal(invalidCommand))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Password does not meet requirements");

        verify(passwordValidator).validate(invalidPassword);
        verify(passwordEncoder, never()).matches(anyString(), anyString());
        verify(passwordEncoder, never()).encode(anyString());
    }
}
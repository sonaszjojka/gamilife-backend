package unittests;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.api.user.dto.RegisterUserApiDto;
import edu.pjwstk.auth.usecase.registeruser.RegisterUserCommand;
import edu.pjwstk.auth.usecase.registeruser.RegisterUserUseCaseImpl;
import edu.pjwstk.auth.validators.PasswordValidator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.time.LocalDate;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class RegisterUserUseCaseTest {

    @Mock
    private PasswordEncoder passwordEncoder;

    @Mock
    private PasswordValidator passwordValidator;

    @Mock
    private UserApi userApi;

    @InjectMocks
    private RegisterUserUseCaseImpl registerUserUseCase;

    private RegisterUserCommand command;
    private static final String FIRST_NAME = "John";
    private static final String LAST_NAME = "Doe";
    private static final String EMAIL = "john.doe@example.com";
    private static final String PASSWORD = "SecurePassword123!";
    private static final String ENCODED_PASSWORD = "$2a$10$encodedPassword";
    private static final String USERNAME = "johndoe";
    private static final LocalDate DATE_OF_BIRTH = LocalDate.of(1990, 1, 1);
    private static final boolean SEND_BUDGET_REPORTS = true;
    private static final boolean IS_PROFILE_PUBLIC = false;
    private static final UUID USER_ID = UUID.randomUUID();

    @BeforeEach
    void setUp() {
        command = new RegisterUserCommand(
                FIRST_NAME,
                LAST_NAME,
                EMAIL,
                PASSWORD,
                USERNAME,
                DATE_OF_BIRTH,
                SEND_BUDGET_REPORTS,
                IS_PROFILE_PUBLIC
        );
    }

    @Test
    void shouldRegisterUserSuccessfully() {
        // Given
        BasicUserInfoApiDto expectedUser = new BasicUserInfoApiDto(USER_ID, EMAIL, USERNAME);
        RegisterUserApiDto registerUserDto = new RegisterUserApiDto(
                FIRST_NAME,
                LAST_NAME,
                EMAIL,
                ENCODED_PASSWORD,
                USERNAME,
                DATE_OF_BIRTH,
                SEND_BUDGET_REPORTS,
                IS_PROFILE_PUBLIC,
                false
        );

        doNothing().when(passwordValidator).validate(PASSWORD);
        when(passwordEncoder.encode(PASSWORD)).thenReturn(ENCODED_PASSWORD);
        when(userApi.registerNewUser(any(RegisterUserApiDto.class))).thenReturn(expectedUser);

        // When
        BasicUserInfoApiDto result = registerUserUseCase.executeInternal(command);

        // Then
        assertThat(result).isEqualTo(expectedUser);
        assertThat(result.userId()).isEqualTo(USER_ID);
        assertThat(result.email()).isEqualTo(EMAIL);
        assertThat(result.username()).isEqualTo(USERNAME);

        verify(passwordValidator).validate(PASSWORD);
        verify(passwordEncoder).encode(PASSWORD);
        verify(userApi).registerNewUser(argThat(dto ->
                dto.firstName().equals(FIRST_NAME) &&
                        dto.lastName().equals(LAST_NAME) &&
                        dto.email().equals(EMAIL) &&
                        dto.password().equals(ENCODED_PASSWORD) &&
                        dto.username().equals(USERNAME) &&
                        dto.dateOfBirth().equals(DATE_OF_BIRTH) &&
                        dto.sendBudgetReports() == SEND_BUDGET_REPORTS &&
                        dto.isProfilePublic() == IS_PROFILE_PUBLIC &&
                        !dto.isEmailVerified()
        ));
    }

    @Test
    void shouldThrowExceptionWhenPasswordValidationFails() {
        // Given
        String invalidPassword = "weak";
        RegisterUserCommand invalidCommand = new RegisterUserCommand(
                FIRST_NAME,
                LAST_NAME,
                EMAIL,
                invalidPassword,
                USERNAME,
                DATE_OF_BIRTH,
                SEND_BUDGET_REPORTS,
                IS_PROFILE_PUBLIC
        );

        doThrow(new IllegalArgumentException("Password does not meet requirements"))
                .when(passwordValidator).validate(invalidPassword);

        // When & Then
        assertThatThrownBy(() -> registerUserUseCase.executeInternal(invalidCommand))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Password does not meet requirements");

        verify(passwordValidator).validate(invalidPassword);
        verify(passwordEncoder, never()).encode(anyString());
        verify(userApi, never()).registerNewUser(any());
    }

    @Test
    void shouldThrowExceptionWhenUserApiFailsToRegister() {
        // Given
        doNothing().when(passwordValidator).validate(PASSWORD);
        when(passwordEncoder.encode(PASSWORD)).thenReturn(ENCODED_PASSWORD);
        when(userApi.registerNewUser(any(RegisterUserApiDto.class)))
                .thenThrow(new RuntimeException("User already exists"));

        // When & Then
        assertThatThrownBy(() -> registerUserUseCase.executeInternal(command))
                .isInstanceOf(RuntimeException.class)
                .hasMessage("User already exists");

        verify(passwordValidator).validate(PASSWORD);
        verify(passwordEncoder).encode(PASSWORD);
        verify(userApi).registerNewUser(any(RegisterUserApiDto.class));
    }
}
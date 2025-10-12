package edu.pjwstk.application.exception;


import edu.pjwstk.pomodoro.exception.InvalidPomodoroTaskData;
import edu.pjwstk.pomodoro.exception.PomodoroTaskNotFound;
import edu.pjwstk.tasks.exception.*;
import org.springframework.http.ProblemDetail;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.nio.file.AccessDeniedException;
import java.security.InvalidParameterException;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.stream.Collectors;

@RestControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ProblemDetail handleValidation(MethodArgumentNotValidException ex) {
        List<String> errors = ex.getBindingResult()
                .getFieldErrors()
                .stream()
                .map(error -> error.getDefaultMessage() == null ? "Validation error" : error.getDefaultMessage())
                .collect(Collectors.toList());
        ProblemDetail problem = formatErrorResponse(ErrorCode.VALIDATION_ERROR, "Validation failed");
        problem.setProperty("errors", errors);
        return problem;
    }

    @ExceptionHandler({
            DateTimeParseException.class,
            InvalidParameterException.class
    })
    public ProblemDetail handleValidation(Exception ex) {
        ProblemDetail problem = formatErrorResponse(ErrorCode.VALIDATION_ERROR, "Validation failed");
        problem.setProperty("errors", List.of(ex.getMessage()));
        return problem;
    }

    @ExceptionHandler(AccessDeniedException.class)
    public ProblemDetail handleAccessDenied(AccessDeniedException ex) {
        return formatErrorResponse(ErrorCode.ACCESS_DENIED, ex.getMessage());
    }

    //
//    @ExceptionHandler(EmailLinkedThroughProviderException.class)
//    public ProblemDetail handleEmailLinked(EmailLinkedThroughProviderException ex) {
//        ProblemDetail problem = formatErrorResponse(ErrorCode.EMAIL_LINKED_THROUGH_PROVIDER, ex.getMessage());
//        problem.setProperty("linkToken", ex.getLinkToken());
//        return problem;
//    }
//
//    @ExceptionHandler(InvalidCredentialsException.class)
//    public ProblemDetail handleInvalidPassword(InvalidCredentialsException ex) {
//        return formatErrorResponse(ErrorCode.INVALID_CREDENTIALS, ex.getMessage());
//    }
//
//    @ExceptionHandler(InvalidLinkTokenException.class)
//    public ProblemDetail handleInvalidLinkToken(InvalidLinkTokenException ex) {
//        return formatErrorResponse(ErrorCode.INVALID_LINK_TOKEN, ex.getMessage());
//    }
//
//    @ExceptionHandler(InvalidRegistrationTokenException.class)
//    public ProblemDetail handleInvalidRegistrationToken(InvalidRegistrationTokenException ex) {
//        return formatErrorResponse(ErrorCode.INVALID_REGISTRATION_TOKEN, ex.getMessage());
//    }
//
//    @ExceptionHandler(RefreshTokenExpiredException.class)
//    public ProblemDetail handleRefreshTokenExpired(RefreshTokenExpiredException ex) {
//        return formatErrorResponse(ErrorCode.REFRESH_TOKEN_EXPIRED, ex.getMessage());
//    }
//
//    @ExceptionHandler(RefreshTokenUnknownException.class)
//    public ProblemDetail handleRefreshTokenNotFound(RefreshTokenUnknownException ex) {
//        return formatErrorResponse(ErrorCode.REFRESH_TOKEN_UNKNOWN, ex.getMessage());
//    }
//
//    @ExceptionHandler(RefreshTokenRevokedException.class)
//    public ProblemDetail handleRefreshTokenRevoked(RefreshTokenRevokedException ex) {
//        return formatErrorResponse(ErrorCode.REFRESH_TOKEN_REVOKED, ex.getMessage());
//    }
//
//    @ExceptionHandler(UserAlreadyExistsException.class)
//    public ProblemDetail handleUserAlreadyExists(UserAlreadyExistsException ex) {
//        return formatErrorResponse(ErrorCode.USER_ALREADY_EXISTS, ex.getMessage());
//    }
//
//    @ExceptionHandler(UserNotFoundException.class)
//    public ProblemDetail handleUserNotFound(UserNotFoundException ex) {
//        return formatErrorResponse(ErrorCode.USER_NOT_FOUND, ex.getMessage());
//    }
//
//    @ExceptionHandler(LinkedUserNotFoundException.class)
//    public ProblemDetail handleLinkedUserNotFound(LinkedUserNotFoundException ex) {
//        return formatErrorResponse(ErrorCode.LINKED_USER_NOT_FOUND, ex.getMessage());
//    }
//
//    @ExceptionHandler(AccessTokenExpiredException.class)
//    public ProblemDetail handleAccessTokenExpired(AccessTokenExpiredException ex) {
//        return formatErrorResponse(ErrorCode.ACCESS_TOKEN_EXPIRED, ex.getMessage());
//    }
//
//    @ExceptionHandler(EmptyAccessTokenException.class)
//    public ProblemDetail handleEmptyAccessToken(EmptyAccessTokenException ex) {
//        return formatErrorResponse(ErrorCode.EMPTY_ACCESS_TOKEN, ex.getMessage());
//    }
//
//    @ExceptionHandler(InvalidAccessTokenException.class)
//    public ProblemDetail handleInvalidAccessToken(InvalidAccessTokenException ex) {
//        return formatErrorResponse(ErrorCode.INVALID_ACCESS_TOKEN, ex.getMessage());
//    }
//
//    @ExceptionHandler(UnsupportedAccessTokenException.class)
//    public ProblemDetail handleUnsupportedAccessTokenType(UnsupportedAccessTokenException ex) {
//        return formatErrorResponse(ErrorCode.UNSUPPORTED_ACCESS_TOKEN_TYPE, ex.getMessage());
//    }
//
//    @ExceptionHandler(RefreshTokenNotProvidedException.class)
//    public ProblemDetail handleRefreshTokenNotProvided(RefreshTokenNotProvidedException ex) {
//        return formatErrorResponse(ErrorCode.INVALID_REFRESH_TOKEN, ex.getMessage());
//    }
//
//    @ExceptionHandler(MissingRequestCookieException.class)
//    public ProblemDetail handleMissingCookie(MissingRequestCookieException ex) {
//        return formatErrorResponse(ErrorCode.MISSING_REFRESH_TOKEN_COOKIE, ex.getMessage());
//    }

    @ExceptionHandler(InvalidHabitDataException.class)
    public ProblemDetail handleInvalidHabitData(InvalidHabitDataException ex) {
        return formatErrorResponse(ErrorCode.INVALID_HABIT_DATA, ex.getMessage());
    }

    @ExceptionHandler(TaskDifficultyNotFoundException.class)
    public ProblemDetail handleTaskDifficultyNotFound(TaskDifficultyNotFoundException ex) {
        return formatErrorResponse(ErrorCode.TASK_DIFFICULTY_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(HabitNotFoundException.class)
    public ProblemDetail handleHabitNotFound(HabitNotFoundException ex) {
        return formatErrorResponse(ErrorCode.HABIT_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(TaskNotFoundException.class)
    public ProblemDetail handleTaskNotFound(TaskNotFoundException ex) {
        return formatErrorResponse(ErrorCode.TASK_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(TaskCategoryNotFoundException.class)
    public ProblemDetail handleTaskCategoryNotFound(TaskCategoryNotFoundException ex) {
        return formatErrorResponse(ErrorCode.TASK_CATEGORY_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(InvalidTaskDataException.class)
    public ProblemDetail handleInvalidHabitData(InvalidTaskDataException ex) {
        return formatErrorResponse(ErrorCode.INVALID_TASK_DATA, ex.getMessage());
    }

    @ExceptionHandler(InvalidPomodoroTaskData.class)
    public ProblemDetail handleInvalidPomodoroData(InvalidPomodoroTaskData ex) {
        return formatErrorResponse(ErrorCode.INVALID_POMODORO_TASK_DATA, ex.getMessage());
    }

    @ExceptionHandler(PomodoroTaskNotFound.class)
    public ProblemDetail handlePomodoroTaskNotFound(PomodoroTaskNotFound ex) {
        return formatErrorResponse(ErrorCode.POMODORO_TASK_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(Exception.class)
    public ProblemDetail handleOther() {
        return ErrorCode.INTERNAL_SERVER_ERROR.getProblemDetail();
    }


    private ProblemDetail formatErrorResponse(ErrorCode error, String detail) {
        ProblemDetail problem = error.getProblemDetail();
        problem.setDetail(detail);
        return problem;
    }
}

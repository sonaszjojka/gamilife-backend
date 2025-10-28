package edu.pjwstk.application.exception;


import edu.pjwstk.groups.exception.*;
import edu.pjwstk.pomodoro.exception.InvalidPomodoroTaskData;
import edu.pjwstk.pomodoro.exception.PomodoroTaskNotFound;
import edu.pjwstk.auth.exceptions.*;
import edu.pjwstk.common.authApi.exception.ResetPasswordGenericException;
import edu.pjwstk.common.groupsApi.exception.GroupMemberNotFoundException;
import edu.pjwstk.common.userApi.exception.UserAlreadyExistsException;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.pomodoro.exception.InvalidPomodoroTaskData;
import edu.pjwstk.pomodoro.exception.PomodoroTaskNotFound;
import edu.pjwstk.tasks.exception.*;
import jakarta.validation.ConstraintViolationException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ProblemDetail;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.nio.file.AccessDeniedException;
import java.security.InvalidParameterException;
import java.time.format.DateTimeParseException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@RestControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ProblemDetail handleValidation(MethodArgumentNotValidException ex) {
        Map<String, String> errors = new HashMap<>();
        ex.getBindingResult()
                .getFieldErrors()
                .forEach(
                        error -> errors.put(error.getField(), error.getDefaultMessage() == null
                                ? "Validation error"
                                : error.getDefaultMessage())
                );

        ProblemDetail problem = formatErrorResponse(ErrorCode.VALIDATION_ERROR, "Validation failed");
        problem.setProperty("errors", errors);
        return problem;
    }

    @ExceptionHandler(ConstraintViolationException.class)
    public ProblemDetail handleConstraintValidationException(ConstraintViolationException ex) {
        Map<String, String> errors = new HashMap<>();
        ex.getConstraintViolations()
                .forEach(
                        cv -> errors.put(
                                cv.getPropertyPath()
                                        .toString()
                                        .substring(cv.getPropertyPath().toString().lastIndexOf(".") + 1),
                                cv.getMessage())
                );

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

    @ExceptionHandler(InvalidCredentialsException.class)
    public ProblemDetail handleInvalidPassword(InvalidCredentialsException ex) {
        return formatErrorResponse(ErrorCode.INVALID_CREDENTIALS, ex.getMessage());
    }

    @ExceptionHandler(RefreshTokenExpiredException.class)
    public ProblemDetail handleRefreshTokenExpired(RefreshTokenExpiredException ex) {
        return formatErrorResponse(ErrorCode.REFRESH_TOKEN_EXPIRED, ex.getMessage());
    }

    @ExceptionHandler(RefreshTokenUnknownException.class)
    public ProblemDetail handleRefreshTokenNotFound(RefreshTokenUnknownException ex) {
        return formatErrorResponse(ErrorCode.REFRESH_TOKEN_UNKNOWN, ex.getMessage());
    }

    @ExceptionHandler(RefreshTokenRevokedException.class)
    public ProblemDetail handleRefreshTokenRevoked(RefreshTokenRevokedException ex) {
        return formatErrorResponse(ErrorCode.REFRESH_TOKEN_REVOKED, ex.getMessage());
    }

    @ExceptionHandler(UserAlreadyExistsException.class)
    public ProblemDetail handleUserAlreadyExists(UserAlreadyExistsException ex) {
        return formatErrorResponse(ErrorCode.USER_ALREADY_EXISTS, ex.getMessage());
    }

    @ExceptionHandler(UserNotFoundException.class)
    public ProblemDetail handleUserNotFound(UserNotFoundException ex) {
        return formatErrorResponse(ErrorCode.USER_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(LinkedUserNotFoundException.class)
    public ProblemDetail handleLinkedUserNotFound(LinkedUserNotFoundException ex) {
        return formatErrorResponse(ErrorCode.LINKED_USER_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(RefreshTokenNotProvidedException.class)
    public ProblemDetail handleRefreshTokenNotProvided(RefreshTokenNotProvidedException ex) {
        return formatErrorResponse(ErrorCode.INVALID_REFRESH_TOKEN, ex.getMessage());
    }

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

    @ExceptionHandler(GroupMemberNotFoundException.class)
    public ProblemDetail handleInvalidHabitData(GroupMemberNotFoundException ex) {
        return formatErrorResponse(ErrorCode.GROUP_MEMBER_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(org.springframework.security.access.AccessDeniedException.class)
    public ProblemDetail handleInvalidHabitData(org.springframework.security.access.AccessDeniedException ex) {
        return formatErrorResponse(ErrorCode.ACCESS_DENIED, ex.getMessage());
    }

    @ExceptionHandler(GroupTypeNotFoundException.class)
    public ProblemDetail handleGroupTypeNotFoundException(GroupTypeNotFoundException ex) {
        return formatErrorResponse(ErrorCode.GROUP_TYPE_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(InvalidGroupDataException.class)
    public ProblemDetail handleInvalidGroupRequestDataException(InvalidGroupDataException ex) {
        return formatErrorResponse(ErrorCode.INVALID_GROUP_REQUEST_DATA, ex.getMessage());
    }

    @ExceptionHandler(GroupRequestStatusNotFoundException.class)
    public ProblemDetail handleGroupRequestStatusNotFoundException(GroupRequestStatusNotFoundException ex) {
        return formatErrorResponse(ErrorCode.GROUP_REQUEST_STATUS_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(GroupFullException.class)
    public ProblemDetail handleGroupFullException(GroupFullException ex) {
        return formatErrorResponse(ErrorCode.GROUP_IS_FULL, ex.getMessage());
    }

    @ExceptionHandler(GroupRequestNotFoundException.class)
    public ProblemDetail handleGroupRequestNotFoundException(GroupRequestNotFoundException ex) {
        return formatErrorResponse(ErrorCode.GROUP_REQUEST_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(GroupInvitationNotFoundException.class)
    public ProblemDetail handleGroupInvitationNotFoundException(GroupInvitationNotFoundException ex) {
        return formatErrorResponse(ErrorCode.GROUP_INVITATION_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(UserNotGroupAdministratorAccessDeniedException.class)
    public ProblemDetail handleUserNotGroupAdministratorAccessDeniedException(UserNotGroupAdministratorAccessDeniedException ex) {
        return formatErrorResponse(ErrorCode.USER_NOT_GROUP_ADMINISTRATOR_ACCESS_DENIED, ex.getMessage());
    }

    @ExceptionHandler(UserNotOwnerAccessDeniedException.class)
    public ProblemDetail handleUserUserNotOwnerAccessDeniedException(UserNotOwnerAccessDeniedException ex) {
        return formatErrorResponse(ErrorCode.USER_NOT_OWNER_ACCESS_DENIED, ex.getMessage());
    }

    @ExceptionHandler(InvalidGroupInvitationDataException.class)
    public ProblemDetail handleInvalidGroupInvitationDataException(InvalidGroupInvitationDataException ex) {
        return formatErrorResponse(ErrorCode.INVALID_GROUP_INVITATION_DATA, ex.getMessage());
    }

    @ExceptionHandler(InvitationStatusNotFoundException.class)
    public ProblemDetail handleInvitationStatusNotFoundException(InvitationStatusNotFoundException ex) {
        return formatErrorResponse(ErrorCode.INVITATION_STATUS_NOT_FOUND, ex.getMessage());
    }

    @ExceptionHandler(UserAlreadyMemberOfGroupException.class)
    public ProblemDetail handleUserAlreadyMemberOfGroupException(UserAlreadyMemberOfGroupException ex) {
        return formatErrorResponse(ErrorCode.USER_ALREADY_MEMBER_OF_GROUP, ex.getMessage());
    }

    @ExceptionHandler(UserJoinGroupAccessDeniedException.class)
    public ProblemDetail handleUserJoinGroupAccessDeniedException(UserJoinGroupAccessDeniedException ex) {
        return formatErrorResponse(ErrorCode.USER_JOIN_GROUP_ACCESS_DENIED, ex.getMessage());
    }

    @ExceptionHandler(GroupInvitationExpiredException.class)
    public ProblemDetail handleGroupInvitationExpiredException(GroupInvitationExpiredException ex) {
        return formatErrorResponse(ErrorCode.GROUP_INVITATION_EXPIRED, ex.getMessage());
    }

    @ExceptionHandler(UserLeftGroupException.class)
    public ProblemDetail handleUserLeftGroupException(UserLeftGroupException ex) {
        return formatErrorResponse(ErrorCode.USER_LEFT_GROUP, ex.getMessage());
    }

    @ExceptionHandler(AdministratorCannotLeaveGroupException.class)
    public ProblemDetail handleAdministratorCannotLeaveGroupException(AdministratorCannotLeaveGroupException ex) {
        return formatErrorResponse(ErrorCode.ADMIN_CANNOT_LEAVE_GROUP, ex.getMessage());
    }

    @ExceptionHandler(InvalidGroupInvitationTokenException.class)
    public ProblemDetail handleAdministratorCannotLeaveGroupException(InvalidGroupInvitationTokenException ex) {
        return formatErrorResponse(ErrorCode.INVALID_GROUP_INVITATION_TOKEN, ex.getMessage());
    }

    @ExceptionHandler(ResetPasswordGenericException.class)
    public ProblemDetail handleResetPasswordGeneric(ResetPasswordGenericException ex) {
        return formatErrorResponse(ErrorCode.PASSWORD_RESET_FAILED, ex.getMessage());
    }

    @ExceptionHandler(OldAndNewPasswordAreTheSameException.class)
    public ProblemDetail handleOldAndNewPasswordAreTheSame(OldAndNewPasswordAreTheSameException ex) {
        return formatErrorResponse(ErrorCode.OLD_AND_NEW_PASSWORD_ARE_SAME, ex.getMessage());
    }

    @ExceptionHandler(Exception.class)
    public ProblemDetail handleOther(Exception ex) {
        log.error(ex.getMessage(), ex);
        return ErrorCode.INTERNAL_SERVER_ERROR.getProblemDetail();
    }

    private ProblemDetail formatErrorResponse(ErrorCode error, String detail) {
        ProblemDetail problem = error.getProblemDetail();
        problem.setDetail(detail);
        return problem;
    }
}

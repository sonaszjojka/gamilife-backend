package edu.pjwstk.application.exception;

import edu.pjwstk.pomodoro.exception.PomodoroTaskNotFound;
import org.springframework.http.HttpStatus;

public enum ErrorCode {

    VALIDATION_ERROR(ErrorCode.VALIDATION_ERROR_CODE, "Validation Error", HttpStatus.BAD_REQUEST),
    ACCESS_DENIED(ErrorCode.ACCESS_DENIED_CODE, "Access Denied", HttpStatus.FORBIDDEN),
    EMAIL_LINKED_THROUGH_PROVIDER(ErrorCode.EMAIL_LINKED_THROUGH_PROVIDER_CODE, "Email is already linked through another provider", HttpStatus.CONFLICT),
    EMAIL_USED_BY_DIFFERENT_PROVIDER(ErrorCode.EMAIL_LINKED_THROUGH_LOCAL_CODE, "Email is already used for account with different provider", HttpStatus.CONFLICT),
    INVALID_CREDENTIALS(ErrorCode.INVALID_CREDENTIALS_CODE, "Invalid Credentials", HttpStatus.UNAUTHORIZED),
    INVALID_LINK_TOKEN(ErrorCode.INVALID_LINK_TOKEN_CODE, "Invalid Link Token", HttpStatus.BAD_REQUEST),
    INVALID_REGISTRATION_TOKEN(ErrorCode.INVALID_REGISTRATION_TOKEN_CODE, "Invalid Registration Token", HttpStatus.BAD_REQUEST),
    REFRESH_TOKEN_EXPIRED(ErrorCode.REFRESH_TOKEN_EXPIRED_CODE, "Refresh Token Expired", HttpStatus.UNAUTHORIZED),
    REFRESH_TOKEN_UNKNOWN(ErrorCode.REFRESH_TOKEN_UNKNOWN_CODE, "Refresh Token Unknown", HttpStatus.UNAUTHORIZED),
    REFRESH_TOKEN_REVOKED(ErrorCode.REFRESH_TOKEN_REVOKED_CODE, "Refresh Token Revoked", HttpStatus.UNAUTHORIZED),
    USER_ALREADY_EXISTS(ErrorCode.USER_ALREADY_EXISTS_CODE, "User Already Exists", HttpStatus.CONFLICT),
    USER_NOT_FOUND(ErrorCode.USER_NOT_FOUND_CODE, "User Not Found", HttpStatus.NOT_FOUND),
    LINKED_USER_NOT_FOUND(ErrorCode.LINKED_USER_NOT_FOUND_CODE, "Linked User Not Found", HttpStatus.NOT_FOUND),
    ACCESS_TOKEN_EXPIRED(ErrorCode.ACCESS_TOKEN_EXPIRED_CODE, "Access Token Expired", HttpStatus.UNAUTHORIZED),
    EMPTY_ACCESS_TOKEN(ErrorCode.EMPTY_ACCESS_TOKEN_CODE, "Empty Access Token", HttpStatus.BAD_REQUEST),
    INVALID_ACCESS_TOKEN(ErrorCode.INVALID_ACCESS_TOKEN_CODE, "Invalid Access Token", HttpStatus.UNAUTHORIZED),
    UNSUPPORTED_ACCESS_TOKEN_TYPE(ErrorCode.UNSUPPORTED_ACCESS_TOKEN_TYPE_CODE, "Unsupported Access Token Type", HttpStatus.BAD_REQUEST),
    ADDITIONAL_REGISTRATION_REQUIRED(ErrorCode.ADDITIONAL_REGISTRATION_REQUIRED_CODE, "Additional Registration Required", HttpStatus.BAD_REQUEST),
    UNAUTHORIZED(ErrorCode.UNAUTHORIZED_CODE, "Unauthorized", HttpStatus.UNAUTHORIZED),
    INVALID_REFRESH_TOKEN(ErrorCode.INVALID_REFRESH_TOKEN_CODE, "Invalid Refresh Token", HttpStatus.UNAUTHORIZED),
    INTERNAL_SERVER_ERROR(ErrorCode.INTERNAL_SERVER_ERROR_CODE, "Internal Server Error", HttpStatus.INTERNAL_SERVER_ERROR),
    MISSING_REFRESH_TOKEN_COOKIE(ErrorCode.MISSING_REFRESH_TOKEN_COOKIE_CODE, "Missing Refresh Token Cookie", HttpStatus.BAD_REQUEST),
    INVALID_HABIT_DATA(ErrorCode.INVALID_HABIT_DATA_CODE, "Invalid Habit Data", HttpStatus.BAD_REQUEST),
    TASK_CATEGORY_NOT_FOUND(ErrorCode.TASK_CATEGORY_NOT_FOUND_CODE, "Task Category Not Found", HttpStatus.NOT_FOUND),
    TASK_DIFFICULTY_NOT_FOUND(ErrorCode.TASK_DIFFICULTY_NOT_FOUND_CODE, "Task Difficulty Not Found", HttpStatus.NOT_FOUND),
    HABIT_NOT_FOUND(ErrorCode.HABIT_NOT_FOUND_CODE, "Habit Not Found", HttpStatus.NOT_FOUND),
    TASK_NOT_FOUND(ErrorCode.TASK_NOT_FOUND_CODE, "Task Not Found", HttpStatus.NOT_FOUND),
    INVALID_TASK_DATA(ErrorCode.INVALID_TASK_DATA_CODE, "Invalid Task Data", HttpStatus.BAD_REQUEST),
    INVALID_POMODORO_TASK_DATA(ErrorCode.INVALID_POMODORO_TASK_DATA_CODE, "Invalid Pomodoro Task Data", HttpStatus.BAD_REQUEST),
    POMODORO_TASK_NOT_FOUND(ErrorCode.POMODORO_TASK_NOT_FOUND_CODE, "Pomodoro Task Not Found", HttpStatus.NOT_FOUND),
    GROUP_MEMBER_NOT_FOUND(ErrorCode.GROUP_MEMBER_NOT_FOUND_CODE, "Group Member Not Found", HttpStatus.NOT_FOUND),
    GROUP_TYPE_NOT_FOUND(ErrorCode.GROUP_TYPE_NOT_FOUND_CODE, "Group Type Not Found", HttpStatus.NOT_FOUND),
    INVALID_GROUP_REQUEST_DATA(ErrorCode.INVALID_GROUP_REQUEST_DATA_CODE, "Invalid Group Request Data", HttpStatus.BAD_REQUEST),
    GROUP_REQUEST_STATUS_NOT_FOUND(ErrorCode.GROUP_REQUEST_STATUS_NOT_FOUND_CODE, "Group Request Status Not Found", HttpStatus.NOT_FOUND),
    GROUP_IS_FULL(ErrorCode.GROUP_IS_FULL_CODE, "Group Is Full – Member Limit Reached", HttpStatus.UNPROCESSABLE_ENTITY),
    GROUP_REQUEST_NOT_FOUND(ErrorCode.GROUP_REQUEST_NOT_FOUND_CODE, "Group Request Not Found", HttpStatus.NOT_FOUND),
    GROUP_INVITATION_NOT_FOUND(ErrorCode.GROUP_INVITATION_NOT_FOUND_CODE, "Group Invitation Not Found", HttpStatus.NOT_FOUND),
    USER_NOT_GROUP_ADMINISTRATOR_ACCESS_DENIED(ErrorCode.USER_NOT_GROUP_ADMINISTRATOR_ACCESS_DENIED_CODE, "User Not Group Administrator Access Denied", HttpStatus.FORBIDDEN),
    USER_NOT_OWNER_ACCESS_DENIED(ErrorCode.USER_NOT_OWNER_ACCESS_DENIED_CODE, "User Not Owner Access Denied", HttpStatus.FORBIDDEN),
    INVALID_GROUP_INVITATION_DATA(ErrorCode.INVALID_GROUP_INVITATION_DATA_CODE, "Invalid Group Invitation Data", HttpStatus.BAD_REQUEST),
    INVITATION_STATUS_NOT_FOUND(ErrorCode.INVITATION_STATUS_NOT_FOUND_CODE, "Invitation Status Not Found", HttpStatus.NOT_FOUND);

    public static final int VALIDATION_ERROR_CODE = 1001;
    public static final int ACCESS_DENIED_CODE = 1002;
    public static final int EMAIL_LINKED_THROUGH_PROVIDER_CODE = 1003;
    public static final int EMAIL_LINKED_THROUGH_LOCAL_CODE = 1004;
    public static final int INVALID_CREDENTIALS_CODE = 1005;
    public static final int INVALID_LINK_TOKEN_CODE = 1006;
    public static final int INVALID_REGISTRATION_TOKEN_CODE = 1007;
    public static final int REFRESH_TOKEN_EXPIRED_CODE = 1008;
    public static final int REFRESH_TOKEN_UNKNOWN_CODE = 1009;
    public static final int REFRESH_TOKEN_REVOKED_CODE = 1010;
    public static final int USER_ALREADY_EXISTS_CODE = 1011;
    public static final int USER_NOT_FOUND_CODE = 1012;
    public static final int LINKED_USER_NOT_FOUND_CODE = 1013;
    public static final int ACCESS_TOKEN_EXPIRED_CODE = 1014;
    public static final int EMPTY_ACCESS_TOKEN_CODE = 1015;
    public static final int INVALID_ACCESS_TOKEN_CODE = 1016;
    public static final int UNSUPPORTED_ACCESS_TOKEN_TYPE_CODE = 1017;
    public static final int ADDITIONAL_REGISTRATION_REQUIRED_CODE = 1018;
    public static final int UNAUTHORIZED_CODE = 1019;
    public static final int INVALID_REFRESH_TOKEN_CODE = 1020;
    public static final int MISSING_REFRESH_TOKEN_COOKIE_CODE = 1021;
    public static final int INTERNAL_SERVER_ERROR_CODE = 5000;
    public static final int INVALID_HABIT_DATA_CODE = 1022;
    public static final int TASK_CATEGORY_NOT_FOUND_CODE = 1023;
    public static final int TASK_DIFFICULTY_NOT_FOUND_CODE = 1024;
    public static final int HABIT_NOT_FOUND_CODE = 1025;
    public static final int TASK_NOT_FOUND_CODE = 1026;
    public static final int INVALID_TASK_DATA_CODE = 1027;
    public static final int INVALID_POMODORO_TASK_DATA_CODE = 1028;
    public static final int POMODORO_TASK_NOT_FOUND_CODE = 1029;
    public static final int GROUP_MEMBER_NOT_FOUND_CODE = 1030;
    public static final int GROUP_TYPE_NOT_FOUND_CODE = 1031;
    public static final int INVALID_GROUP_REQUEST_DATA_CODE = 1032;
    public static final int GROUP_REQUEST_STATUS_NOT_FOUND_CODE = 1033;
    public static final int GROUP_IS_FULL_CODE = 1034;
    public static final int GROUP_REQUEST_NOT_FOUND_CODE = 1035;
    public static final int GROUP_INVITATION_NOT_FOUND_CODE = 1036;
    public static final int USER_NOT_GROUP_ADMINISTRATOR_ACCESS_DENIED_CODE = 1037;
    public static final int USER_NOT_OWNER_ACCESS_DENIED_CODE = 1038;
    public static final int INVALID_GROUP_INVITATION_DATA_CODE = 1039;
    public static final int INVITATION_STATUS_NOT_FOUND_CODE = 1040;

    private final int code;
    private final String title;
    private final HttpStatus status;

    ErrorCode(int code, String title, HttpStatus status) {
        this.code = code;
        this.title = title;
        this.status = status;
    }

    public int getCode() {
        return code;
    }

    public String getTitle() {
        return title;
    }

    public HttpStatus getStatus() {
        return status;
    }

    public ProblemDetailDto getProblemDetail() {
        return new ProblemDetailDto(status, title, code);
    }
}
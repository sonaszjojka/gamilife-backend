package pl.gamilife.auth.security;

import com.fasterxml.jackson.databind.ObjectMapper;
import pl.gamilife.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.ErrorCode;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.AllArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;
import pl.gamilife.infrastructure.web.exception.ErrorCodesRepository;
import pl.gamilife.infrastructure.web.exception.ErrorResponse;

import java.io.IOException;

@Component
@AllArgsConstructor
public class JwtAuthenticationEntryPoint implements AuthenticationEntryPoint {

    private final ObjectMapper objectMapper;
    private final ErrorCodesRepository errorCodesRepository;

    @Override
    public void commence(
            HttpServletRequest request,
            HttpServletResponse response,
            AuthenticationException authException
    ) throws IOException {

        ErrorCode errorCode = AuthErrorCode.UNAUTHORIZED;
        ErrorCodesRepository.ErrorDefinition errorDefinition =
                errorCodesRepository.get(errorCode.getKey());

        ErrorResponse errorResponse = ErrorResponse.of(
                errorDefinition.getStatus(),
                "https://gamilife.pl/errors/"
                        + errorCode.getModule().toLowerCase().replace("_", "-") + "/"
                        + errorCode.getKey().toLowerCase().replace("_", "-"),
                errorDefinition.getTitle(),
                errorDefinition.getDetail(),
                request.getRequestURI()
        );
        errorResponse.setCode(String.valueOf(errorDefinition.getCode()));

        response.setStatus(errorDefinition.getStatus());
        response.setContentType(MediaType.APPLICATION_PROBLEM_JSON_VALUE);
        response.setCharacterEncoding("UTF-8");

        objectMapper.writeValue(response.getOutputStream(), errorResponse);
    }

}

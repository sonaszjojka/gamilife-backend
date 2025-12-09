package pl.gamilife.shared.web.security.annotation;

import org.springframework.security.access.prepost.PreAuthorize;

import java.lang.annotation.*;

/**
 * <p>
 * This annotation ensures that the authenticated user is the same as the one used in the path.
 * It checks if the authenticated user's ID matches the 'userId' parameter in the path.
 * <br/>
 * It is required that the corresponding userId parameter in the path is named <strong>exactly "userId"</strong>
 * </p>
 */
@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@PreAuthorize("authentication.principal.id == #userId")
public @interface AuthenticatedUserIsOwner {
}

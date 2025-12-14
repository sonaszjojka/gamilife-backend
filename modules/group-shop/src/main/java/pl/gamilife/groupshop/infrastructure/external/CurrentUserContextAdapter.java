package pl.gamilife.groupshop.infrastructure.external;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.groupshop.domain.model.projection.GroupShopUser;
import pl.gamilife.groupshop.domain.port.context.CurrentUserContext;

import java.util.UUID;

@Component
@AllArgsConstructor
public class CurrentUserContextAdapter implements CurrentUserContext {
    AuthApi authApi;

    @Override
    public GroupShopUser findGroupShopUserById(UUID userId) {
        return new GroupShopUser(authApi.getCurrentUser().userId());
    }
}

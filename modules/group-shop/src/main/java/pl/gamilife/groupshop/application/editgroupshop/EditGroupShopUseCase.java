package pl.gamilife.groupshop.application.editgroupshop;

import pl.gamilife.groupshop.infrastructure.web.request.EditGroupShopRequest;
import pl.gamilife.shared.kernel.architecture.UseCase;

import java.util.UUID;

public interface EditGroupShopUseCase extends UseCase<EditGroupShopCommand, EditGroupShopResult> { }

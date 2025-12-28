package pl.gamilife.groupshop.application.createownedgroupitem;


import pl.gamilife.shared.kernel.architecture.UseCase;

public interface CreateOwnedGroupItemUseCase extends UseCase<CreateOwnedGroupItemCommand, CreateOwnedGroupItemResult> {

    CreateOwnedGroupItemResult execute(CreateOwnedGroupItemCommand cmd);
}

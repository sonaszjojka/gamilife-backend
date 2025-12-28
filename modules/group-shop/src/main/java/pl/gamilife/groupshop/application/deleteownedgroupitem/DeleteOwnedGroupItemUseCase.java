package pl.gamilife.groupshop.application.deleteownedgroupitem;


import pl.gamilife.shared.kernel.architecture.UseCase;

public interface DeleteOwnedGroupItemUseCase extends UseCase<DeleteOwnedGroupItemCommand, Void> {

    Void execute(DeleteOwnedGroupItemCommand cmd);
}

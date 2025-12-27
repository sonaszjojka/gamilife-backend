package pl.gamilife.groupshop.application.deletegroupitem;

import pl.gamilife.shared.kernel.architecture.UseCase;

public interface DeleteGroupItemUseCase extends UseCase<DeleteGroupItemCommand, Void> {
    Void execute(DeleteGroupItemCommand cmd);
}

package pl.gamilife.groupshop.application.editgroupitem;


import pl.gamilife.shared.kernel.architecture.UseCase;

public interface EditGroupItemUseCase extends UseCase<EditGroupItemCommand, EditGroupItemResult> {

    EditGroupItemResult execute(EditGroupItemCommand cmd);
}

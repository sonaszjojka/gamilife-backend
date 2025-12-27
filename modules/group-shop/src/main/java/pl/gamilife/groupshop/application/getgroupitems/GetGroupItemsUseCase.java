package pl.gamilife.groupshop.application.getgroupitems;

import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface GetGroupItemsUseCase extends UseCase<GetGroupItemsCommand, Page<GetGroupItemResult>> {

    Page<GetGroupItemResult> execute(GetGroupItemsCommand command);
}

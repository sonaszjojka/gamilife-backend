package pl.gamilife.groupshop.application.getownedgroupitems;

import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface GetOwnedGroupItemsUseCase extends UseCase<GetOwnedGroupItemsCommand, Page<GetOwnedGroupItemsResult>> {

   Page<GetOwnedGroupItemsResult> execute(GetOwnedGroupItemsCommand cmd);
}

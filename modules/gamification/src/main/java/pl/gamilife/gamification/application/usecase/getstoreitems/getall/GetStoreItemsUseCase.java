package pl.gamilife.gamification.application.usecase.getstoreitems.getall;

import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface GetStoreItemsUseCase extends UseCase<GetStoreItemsCommand, Page<StoreItemDto>> {
}

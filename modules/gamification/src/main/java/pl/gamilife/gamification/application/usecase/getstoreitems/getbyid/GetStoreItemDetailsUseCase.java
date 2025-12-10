package pl.gamilife.gamification.application.usecase.getstoreitems.getbyid;


import java.util.UUID;

public interface GetStoreItemDetailsUseCase {

     StoreItemDetailsDto execute(UUID itemId,UUID userId);
}

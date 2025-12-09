package pl.gamilife.gamification.application.usecase.getstoreitems.getbyid;


import java.util.UUID;

public interface GetStoreItemDetailsUseCase {

    public StoreItemDetailsDto execute(UUID itemId);
}

package pl.gamilife.gamification.usecase.getallitemrarities;

import java.util.List;

public record GetAllItemRaritiesResult(
        List<ItemRarityDTO> itemRarities
) {
    public record ItemRarityDTO(
            Integer id,
            String name
    ) {
    }
}
